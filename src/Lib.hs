{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Lib
    ( someFunc
    ) where

import Network.Wreq
import Text.HTML.Scalpel
import Control.Lens
import qualified Data.ByteString.Lazy as LBS
import Control.Exception as E
import Network.HTTP.Client

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
      shrinkRange (from, to) =
        if succ from >= to
          then return from
          else do
            let mid = (from + to) `div` 2
            isMidThereYet <- oracle mid
            if isMidThereYet
              then shrinkRange (mid, to)
              else shrinkRange (from, mid)
  in do
    rng <- guessRange 1
    shrinkRange rng

isShkoloOk :: ShkoloUrl -> IO Bool
isShkoloOk url = do
    putStrLn url
    (get url >> return True) `E.catch` handler
  where
    handler e@(StatusCodeException s _ _)
      | s ^. statusCode == 404 = return False
      | otherwise              = throwIO e
    handler e                  = throwIO e
