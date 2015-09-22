{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types
    ( ShkoloUrl
    , ArticleMetaData(..)
    , parseTime'
    ) where

import Data.Time
import qualified Data.Text.Lazy as LT
import Data.Aeson
import GHC.Generics
import Data.Csv
import Control.Monad
import Data.Functor.Identity

type ShkoloUrl = String

data ArticleMetaData = ArticleMetaData
    { _title :: LT.Text
    , _publishedAt :: ZonedTime
    , _link :: LT.Text
    , _newsId :: LT.Text
    , _author :: LT.Text
    } deriving (Show, Generic)

instance ToJSON ArticleMetaData

instance FromRecord ArticleMetaData where
  parseRecord v
    | length v == 3 =
        ArticleMetaData
          <$>  v .! 0
          <*> (v .! 1 >>= parseTime'')
          <*>  v .! 2
          <*>  v .! 3
          <*>  v .! 4
    | otherwise = mzero


instance ToRecord ArticleMetaData where
  toRecord ArticleMetaData{..} = record [
    toField _title,
    toField (unparseTime _publishedAt),
    toField _link,
    toField _newsId,
    toField _author
    ]

parseTime'' :: (Monad m, ParseTime t) => LT.Text -> m t
parseTime'' = parseTimeM False defaultTimeLocale fmt . LT.unpack

parseTime' :: ParseTime t => LT.Text -> t
parseTime' = runIdentity . parseTime''

unparseTime :: ZonedTime -> LT.Text
unparseTime = LT.pack . formatTime defaultTimeLocale fmt

fmt :: String
fmt = "%Y-%m-%dT%H:%M:%S%z"
