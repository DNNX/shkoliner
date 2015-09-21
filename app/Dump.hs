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
  forM_ args $ \category -> do
    -- kinda hiding from search engines
    articles <- scrapeAll ("http://" ++ category ++ reverse "yb.renilno.")
    dumpMetas articles (category ++ ".csv")
