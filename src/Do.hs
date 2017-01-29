{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings, ConstraintKinds, GADTs, KindSignatures, DataKinds, ScopedTypeVariables, TypeOperators, TupleSections,  RankNTypes, TypeApplications,RankNTypes #-}

module Do where

import Control.Monad
import Data.List

import Criterion.Main

import Test.QuickCheck

import Data.Traversable

import Data.Constraint
import Data.Kind

import Test.QuickCheck
import Test.QuickCheck.Function

import qualified Text.Parsec as P
import qualified Text.Parsec as P
import Data.Dates (DateTime, pDateTime, parseDate,getCurrentDateTime)

import Text.Printf
import Control.Parallel.Strategies
import System.IO.Unsafe
import Data.Time

{-# LANGUAGE FlexibleContexts #-}


import Data.Conduit
import Data.CSV.Conduit
import Data.Vector
import Data.ByteString
import Data.Conduit.Binary
import Data.Conduit.List as CL
import Data.ByteString.Lex.Fractional
import Data.Text.Encoding
import Data.Text.Read
import Data.Either

getValues :: FilePath -> IO [ByteString]
getValues file = do
    -- curl -O https://bitbucket.org/hyllos/effectus-python/raw/2e0954599c97d2b7211807df1076800b7187ced4/effectus/csv/wkzs.csv
    v <- readCSVFile defCSVSettings file :: IO (Vector (Row ByteString))
    -- v is like [["56.56145325"],["99.42876163"], ..
    pure $ Prelude.map (Prelude.head) $ toList v

bb = rights . fmap  convert <$> getValues "wkzs.csv"
  where
    convert s = case double (decodeUtf8 s) of
        Right (d, "") -> Right d
        _             -> Left "myError" -- merijin: alternatively just return a "default" here and the number above, to remove "rights" entirel

cost :: [[Double]] -> [[Double]] -> Double
cost [[]] [[]] = 0
cost ((x:xs):xss) ((y:ys):yss) = (x-y)^2 + cost [xs] [ys] + cost xss yss
cost _ _ = 0

foo = print (cost [[1,2,3],[4,5,6]] [[1,3,3],[4,6,6]]) -- 2.0

--
