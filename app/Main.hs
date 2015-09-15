module Main where

import Lib

main :: IO ()
main = do
  x <- numPages "http://..."
  print x
