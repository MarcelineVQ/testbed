{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Weigh

-- | Weigh integers.
main :: IO ()
main =
  mainWith (do integers
               ints
               maxMB)

-- | Just counting integers.
integers :: Weigh ()
integers =
  do func "integers count 0" count 0
     func "integers count 1" count 1
     func "integers count 2" count 2
     func "integers count 3" count 3
     func "integers count 10" count 10
     func "integers count 100" count 100
  where count :: Integer -> ()
        count 0 = ()
        count a = count (a - 1)

-- | We count ints and ensure that the allocations are optimized away
-- to only two 64-bit Ints (16 bytes).
ints :: Weigh ()
ints =
  do validateFunc "ints count 1" count 1 (maxAllocs 24)
     validateFunc "ints count 10" count 10 (maxAllocs 24)
     validateFunc "ints count 1000000" count 1000000 (maxAllocs 24)
  where count :: Int -> ()
        count 0 = ()
        count a = count (a - 1)

-- | Comparing residency between a strict fold and a lazy one.
-- Lazy should fail the limit.
maxMB :: Weigh ()
maxMB =
  do validateFunc "strict fold" (lfold' (+) 0) list $ maxLiveMB 120 -- MB
     validateFunc "lazy fold" (lfold (+) 0) list $ shouldFail (maxLiveMB 120)
  where
    list = [1..1000000 :: Int]
    lfold _ z [] = z; lfold f z (x:xs) =
      lfold f (f z x) xs
    lfold' _ a [] = a; lfold' f a (x:xs) =
      let a' = f a x in a' `seq` lfold' f a' xs
