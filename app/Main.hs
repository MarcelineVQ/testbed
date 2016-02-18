module Main where

--import Lib
import Control.Concurrent

main :: IO ()
main = do
  g <- getNumCapabilities
  print g
