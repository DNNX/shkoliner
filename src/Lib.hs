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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.ByteString.Base64 as Base64
import System.FilePath
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

cachedGet :: ShkoloUrl -> IO BS.ByteString
cachedGet =
  cached
    (\url ->  "./cache" </> fsKey url)
    (\url -> LBS.toStrict . view responseBody <$> get url)

cached :: (k -> FilePath) -> (k -> IO BS.ByteString) -> k -> IO BS.ByteString
cached toFilePath load key = do
  let fPath = toFilePath key
  cachedAlready <- doesFileExist fPath
  if cachedAlready
    then BS.readFile fPath
    else do
      d <- load key
      BS.writeFile fPath d
      return d

fsKey :: ShkoloUrl -> FilePath
fsKey = BS8.toString . Base64.encode . BS8.fromString

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
  bytes :: BS.ByteString <- cachedGet url
  return $ extractArticles $ parseTags bytes

extractArticles :: [Tag BS.ByteString] -> [ArticleMetaData]
extractArticles allTags =
  let articleSections = sections articleTag allTags
      articleTag :: Tag BS.ByteString -> Bool
      articleTag tag =
        case tag of
          TagOpen "article" attrs -> "b-posts-1-item" `BS.isPrefixOf` fromMaybe BS.empty (Prelude.lookup "class" attrs)
          _                       -> False
  in fmap extractArticle articleSections

extractArticle :: [Tag BS.ByteString] -> ArticleMetaData
extractArticle tags =
  ArticleMetaData
    (extractTitle tags)
    (extractPubAt tags)
    (extractLink tags)
    (extractNewsId tags)
    (extractAuthor tags)

extractTitle :: [Tag BS.ByteString] -> T.Text
extractTitle = T.decodeUtf8 . innerText . takeBetween "<span>" "</span>" . takeBetween "<h3 class=\"b-posts-1-item__title\">" "</h3>"

extractPubAt :: [Tag BS.ByteString] -> ZonedTime
extractPubAt = parseTime' . T.decodeUtf8 . fromAttrib "datetime" . head . takeBetween "<time>" "</time>"

extractLink :: [Tag BS.ByteString] -> T.Text
extractLink = T.decodeUtf8 . fromAttrib "href" . head . takeBetween "<a>" "</a>" . takeBetween "<h3 class=\"b-posts-1-item__title\">" "</h3>"

extractNewsId :: [Tag BS.ByteString] -> T.Text
extractNewsId = T.decodeUtf8 . fromAttrib "news_id" . head . takeBetween "<span class=\"show_news_view_count\">" "</span>"

extractAuthor :: [Tag BS.ByteString] -> T.Text
extractAuthor = T.dropAround (\x -> x == '.' || isSpace x) . T.decodeUtf8 . innerText . takeBetween "<span class=\"show_news_view_count\">" "</footer>"

takeBetweens :: String -> String -> [Tag BS.ByteString] -> [[Tag BS.ByteString]]
takeBetweens fromTag toTag = fmap (takeWhile (~/= toTag)) . sections (~== fromTag)

takeBetween :: String -> String -> [Tag BS.ByteString] -> [Tag BS.ByteString]
takeBetween fromTag toTag soup =
  case takeBetweens fromTag toTag soup of
    subSoup:_ -> subSoup
    []        -> error $ "can't find a tag" ++ fromTag ++ " " ++ toTag ++ " " ++ show soup
