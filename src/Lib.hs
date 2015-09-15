module Lib
    ( numPages,
      ShkoloUrl
    ) where

import Network.Wreq
import Control.Lens
import Control.Exception as E
import Network.HTTP.Client

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
    handler e@(StatusCodeException s _ _)
      | s ^. statusCode == 404 = return False
      | otherwise              = throwIO e
    handler e                  = throwIO e
