{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( numPages,
      ShkoloUrl,
      cachedGet,
      ArticleMetaData(..),
      scrapeAll
    ) where

import Network.Wreq
import Control.Lens
import Control.Exception as E
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS8
import qualified Data.ByteString.Base64.Lazy as LBase64
import System.FilePath
import System.Directory
import Data.Time
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Text.HTML.Scalpel
import Data.Maybe

type ShkoloUrl = String

numPages :: ShkoloUrl -> IO Int
numPages baseUrl = whatTheNat $ \page -> do
  isShkoloOk $ pageUrl baseUrl page

pageUrl :: ShkoloUrl -> Int -> ShkoloUrl
pageUrl baseUrl page = baseUrl ++ "/page/" ++ show page

whatTheNat :: Monad m => (Int -> m Bool) -> m Int
whatTheNat oracle =
  let guessRange startAt = do
        areWeThereYet <- oracle startAt
        if areWeThereYet
          then guessRange (startAt * 2)
          else return (startAt `div` 2, startAt)
      shrinkRange (startAt, finishAt) =
        if succ startAt >= finishAt
          then return startAt
          else do
            let mid = (startAt + finishAt) `div` 2
            isMidThereYet <- oracle mid
            if isMidThereYet
              then shrinkRange (mid, finishAt)
              else shrinkRange (startAt, mid)
  in do
    rng <- guessRange 1
    shrinkRange rng

isShkoloOk :: ShkoloUrl -> IO Bool
isShkoloOk url = do
    putStrLn url
    (get url >> return True) `E.catch` handler
  where
    handler e@(HTTP.StatusCodeException s _ _)
      | s ^. statusCode == 404 = return False
      | otherwise              = throwIO e
    handler e                  = throwIO e

cachedGet :: ShkoloUrl -> IO LBS.ByteString
cachedGet url = do
  let k = fsKey url
  let fPath = "./cache" </> k
  cached <- doesFileExist fPath
  if cached
    then LBS.readFile fPath
    else do
      resp <- get url
      let respBody = resp ^. responseBody
      LBS.writeFile fPath respBody
      return respBody

fsKey :: ShkoloUrl -> FilePath
fsKey = LBS8.toString . LBase64.encode . LBS8.fromString

data ArticleMetaData = ArticleMetaData
    { _title :: LT.Text
    , _publishedAt :: UTCTime
    , _link :: LT.Text
    }

scrapeAll :: ShkoloUrl -> IO [ArticleMetaData]
scrapeAll baseUrl = do
  nPages <- numPages baseUrl
  metass <- traverse scrapePage $ fmap (pageUrl baseUrl) [1..nPages]
  return $ concat metass

scrapePage :: ShkoloUrl -> IO [ArticleMetaData]
scrapePage url = do
    bytes :: LBS.ByteString <- cachedGet url
    let txt = LT.decodeUtf8 bytes
    return $ fromJust $ scrapeStringLike txt articles
  where
    articles :: Scraper LT.Text [ArticleMetaData]
    articles = chroots ("article" @: [hasClass "news_for_copy"]) article

    article :: Scraper LT.Text ArticleMetaData
    article = do
      _title <- text $ ("h3" @: [hasClass "b-posts-1-item__title"]) // "span"
      _pubAtStr <- attr "datetime" "time"
      _pubAt <- parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" $ LT.unpack _pubAtStr
      _link <- attr "href" $ ("h3" @: [hasClass "b-posts-1-item__title"]) // "a"
      return $
        ArticleMetaData
          _title
          _pubAt
          _link
