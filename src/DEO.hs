module DEO where

import Criterion.Main
import Data.List
import Data.Ord
import Control.Arrow
import Control.Applicative

doubleEveryOther1 :: [Integer] -> [Integer]
doubleEveryOther1 = reverse . zipWith (*) (cycle [1,2]) . reverse
doubleEveryOther2 = reverse . zipWith ($) (cycle [id,(*2)]) . reverse

doubleEveryOther4 list = foldl (\xs (x,y) -> if even y then x*2:xs else x:xs) [] $ zip (reverse list) [1..]
--doubleEveryOther''''' x = map $ zipWith (*2) (cycle [1,2]) = foldl (\xs (x,y) -> if even y then x*2:xs else x:xs) [] $ zip (reverse [1..5]) [1..]

--fastest
doubleEveryOther3 n@(x:_)
  | even (length n) = zipWith (*) (cycle [2,1]) n
  | otherwise = x : zipWith (*) (cycle [2,1]) (drop 1 n)
doubleEveryOther3 [] = []

doubleEveryOtherW n@(x:_)
  | even (length n) = zipWith ($) (cycle [(*2), id]) n
  | otherwise = x : zipWith ($) (cycle [(*2), id]) (drop 1 n)

doubleEveryOtherR [] = []
doubleEveryOtherR n@(x:_)
  | even (length n) = go n
  | otherwise = x : go (drop 1 n)
  where
    go [] = []
    go [_] = error "go called with odd length somehow"
    go (x':y:zs) = x' * 2 : y : go zs

-- try to double every other in one traversal
-- consider building up a function to do it?

--feh (a,a') (b,b') = (min a b, a' || b')



run :: IO ()
run = defaultMain
  [ bench "doubleEveryOther1" $ whnf doubleEveryOther1 [1..10000]
  , bench "doubleEveryOther2" $ whnf doubleEveryOther2 [1..10000]
  , bench "doubleEveryOther3" $ whnf doubleEveryOther3 [1..10000]
  , bench "doubleEveryOther4" $ whnf doubleEveryOther4 [1..10000]
  , bench "doubleEveryOtherW" $ whnf doubleEveryOtherW [1..10000]
  , bench "doubleEveryOtherR" $ whnf doubleEveryOtherR [1..10000]
  --, bench "doubleEveryOther2" $ whnf doubleEveryOther2 [1..10000]
  ]
