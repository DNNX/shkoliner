{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Text.HTML.TagSoup
import Data.Csv
import Data.Time
import Data.Char
import Data.Maybe

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
  return $ extractArticles $ parseTags bytes

extractArticles :: [Tag LBS.ByteString] -> [ArticleMetaData]
extractArticles allTags =
  let articleSections = sections articleTag allTags
      articleTag :: Tag LBS.ByteString -> Bool
      articleTag tag =
        case tag of
          TagOpen "article" attrs -> "b-posts-1-item" `LBS.isPrefixOf` fromMaybe LBS.empty (Prelude.lookup "class" attrs)
          _                       -> False
  in fmap extractArticle articleSections

extractArticle :: [Tag LBS.ByteString] -> ArticleMetaData
extractArticle tags =
  ArticleMetaData
    (extractTitle tags)
    (extractPubAt tags)
    (extractLink tags)
    (extractNewsId tags)
    (extractAuthor tags)

extractTitle :: [Tag LBS.ByteString] -> LT.Text
extractTitle = LT.decodeUtf8 . innerText . takeBetween "<span>" "</span>" . takeBetween "<h3 class=\"b-posts-1-item__title\">" "</h3>"

extractPubAt :: [Tag LBS.ByteString] -> ZonedTime
extractPubAt = parseTime'' . LT.decodeUtf8 . fromAttrib "datetime" . head . takeBetween "<time>" "</time>"

extractLink :: [Tag LBS.ByteString] -> LT.Text
extractLink = LT.decodeUtf8 . fromAttrib "href" . head . takeBetween "<a>" "</a>" . takeBetween "<h3 class=\"b-posts-1-item__title\">" "</h3>"

extractNewsId :: [Tag LBS.ByteString] -> LT.Text
extractNewsId = LT.decodeUtf8 . fromAttrib "news_id" . head . takeBetween "<span class=\"show_news_view_count\">" "</span>"

extractAuthor :: [Tag LBS.ByteString] -> Maybe LT.Text
extractAuthor = Just . LT.dropAround (\x -> x == '.' || isSpace x) . LT.decodeUtf8 . innerText . takeBetween "<span class=\"show_news_view_count\">" "</footer>"

takeBetweens :: String -> String -> [Tag LBS.ByteString] -> [[Tag LBS.ByteString]]
takeBetweens fromTag toTag = fmap (takeWhile (~/= toTag)) . sections (~== fromTag)

takeBetween :: String -> String -> [Tag LBS.ByteString] -> [Tag LBS.ByteString]
takeBetween fromTag toTag soup =
  case takeBetweens fromTag toTag soup of
    subSoup:_ -> subSoup
    []        -> error $ "can't find a tag" ++ fromTag ++ " " ++ toTag ++ " " ++ show soup
