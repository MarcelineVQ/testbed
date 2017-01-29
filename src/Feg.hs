-- {-# OPTIONS_GHC -Wall #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE GADTsxistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE RecursiveDo #-}

module Feg where

import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Control.Monad.Fix
import Control.Arrow

-- make a < that works for 90 < x < 50

-- given an x :: IO (Set a) and an f :: a -> IO (Set b), how can I create a y :: IO (Set b)?

-- \x y -> (Instr .) . Mov $ x y === \x y -> Instr . Mov x $ y === \x y -> Instr (Mov x y)

--foo,foo',foo'' :: [a] -> [b] -> Instr (Mov [a] [b])
foo,foo',foo'' :: [a] -> [b] -> [a]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x ,_) <- ps]

data Instr a = Instr a
data Mov a b = Mov a b

-- foo   = \x y -> (((Instr .) . Mov)     x) y
-- foo'  = \x y ->   (Instr .    Mov      x) y
-- foo'' = \x y ->    Instr     (Mov      x  y)

foo       = (fst .) . (,)
foo' x    = fst . (,) x
foo'' x y = fst ((,)  x  y)



dabble :: (Ord b) => IO (S.Set a) -> (a -> IO (S.Set b)) -> IO (S.Set b)
dabble x f = do
  -- y = undefined :: IO (S.Set b)
  x >>= fmap S.unions . traverse f . S.toList

fap = do
    x <- [1..10]
    return (f x)
  where
    f = (+1)

data Validation a b = Success b | Failure a deriving (Show, Eq)

instance Functor (Validation a) where
  fmap _ (Failure x) = Failure x
  fmap f (Success x) = Success (f x)


f3 :: a -> a -> a
f3 n1 _ = n1

f3' :: a -> a -> a
f3' _ n2 = n2
