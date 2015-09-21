{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lib

import Data.Foldable
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  traverse_ (\x -> scrapeAll x) args
