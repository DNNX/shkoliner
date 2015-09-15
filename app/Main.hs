module Main where

import Lib

import Data.Foldable
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  traverse_ (\url -> print =<< numPages url) args
