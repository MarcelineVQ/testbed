-- {-# OPTIONS_GHC -Wall #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE GADTsxistentialQuantification #-}
{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

module Feg where

import Prelude hiding ((<))

-- make a < that works for 90 < x < 50

(<) :: (Ord a, Ord b) => a -> b -> Bool
(<) x y = undefined

test = 90 < 10 < 50
test2 = 90 < 100 < 50
test3 = 90 < 10 < 150
test4 = 30 < 40 < 20

data Validation a b = Success b | Failure a deriving (Show, Eq)

instance Functor (Validation a) where
  fmap _ (Failure x) = Failure x
  fmap f (Success x) = Success (f x)


f3 :: a -> a -> a
f3 n1 _ = n1

f3' :: a -> a -> a
f3' _ n2 = n2
