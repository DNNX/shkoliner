{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( numPages,
      ShkoloUrl,
      cachedGet,
      ArticleMetaData(..),
      scrapeAll,
      dumpMetas
    ) where

import Types

import Network.Wreq
import Control.Lens
import Control.Exception as E
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS8
import qualified Data.ByteString.Base64.Lazy as LBase64
import System.FilePath
import System.Directory
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Text.HTML.Scalpel
import Data.Maybe
import Data.Csv

numPages :: ShkoloUrl -> IO Int
numPages baseUrl = whatTheNat $ \page ->
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
cachedGet =
  cached
    (\url ->  "./cache" </> fsKey url)
    (\url -> view responseBody <$> get url)

cached :: (k -> FilePath) -> (k -> IO LBS.ByteString) -> k -> IO LBS.ByteString
cached toFilePath load key = do
  let fPath = toFilePath key
  cachedAlready <- doesFileExist fPath
  if cachedAlready
    then LBS.readFile fPath
    else do
      d <- load key
      LBS.writeFile fPath d
      return d

fsKey :: ShkoloUrl -> FilePath
fsKey = LBS8.toString . LBase64.encode . LBS8.fromString

scrapeAll :: ShkoloUrl -> IO [ArticleMetaData]
scrapeAll baseUrl = do
  nPages <- numPages baseUrl
  metass <- traverse scrapePage $ fmap (pageUrl baseUrl) [1..nPages]
  return $ concat metass

dumpMetas :: [ArticleMetaData] -> FilePath -> IO ()
dumpMetas articles fName =
  LBS.writeFile fName $ Data.Csv.encode articles

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
      _pubAt <- attr "datetime" "time" >>= parseTime'
      _link  <- attr "href" $ ("h3" @: [hasClass "b-posts-1-item__title"]) // "a"
      _newsId <- attr "news_id" ("span" @: [hasClass "news_view_count"])
      _author <- Just <$> text ("span" @: [hasClass "right-side"])
      return $
        ArticleMetaData
          _title
          _pubAt
          _link
          _newsId
          _author
