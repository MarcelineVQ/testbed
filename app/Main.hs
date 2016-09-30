module Main where

--import Lib
import Control.Concurrent

main :: IO ()
main = do
  g <- getNumCapabilities
  print g


data Chew = Chon | Chen deriving (Show, Eq)
