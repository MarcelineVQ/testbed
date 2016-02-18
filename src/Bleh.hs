{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# OPTIONS_GHC -fdefer-typed-holes #-}

module Bleh where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Int
import Data.List

import Data.Tuple.Extra
import Data.Function
import Data.Bifunctor
import Graphics.Gloss
import Data.Data
import Codec.Picture

import Control.Concurrent

import Network
import System.IO

-- data Bore a = Bored a deriving

newtype TotallySafe = TotallySafeGuys (IO ())

fadsa :: [(Int, Int)] -> (Int, Int)
fadsa = maximumBy (compare`on`fst)

data AlgebraEnum = One | Two | Three | Four | Five deriving Data

-- lengthAE :: forall a . a -> Int
--lengthAE x = length . dataTypeConstrs . dataTypeOf $ (undefined :: a)


data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = f a `mappend` foldMap f l `mappend` foldMap f r

behloogle xs = foldr1 (\(x,y) (x',y') -> (min x x', max y y')) xs :: (Int, Int)
behlagle xs = foldr1 (\x y -> ((min`on`fst) x y, (max`on`snd) x y)) xs :: (Int, Int)
behladsfsgle xs = foldr1 (\x y -> ((min`on`fst) x y, (max`on`snd) x y)) xs :: (Int, Int)
behladsfsglfsdfe xs = (minimum (map fst xs), maximum (map snd xs))
behladsfsgdfglfsdfe xs =  (minimum (map fst xs), maximum (map snd xs))
--dffsd tl = foldr1 (\(x,y) (x',y') -> ) tl :: (Int, Int)

applyOverFst :: (a -> b) -> [(a,b)] -> [b]
applyOverFst f xs = map (f . fst) xs
-- applyOverFst f xs = map (f . _) xs

tupleMaxMin :: (Ord a, Ord b) => [(a,b)] -> (a,b)
tupleMaxMin = bimap maximum minimum . unzip

fasd :: Int -> Int -> Int
fasd _ 0 = error "tits"
fast x y = x + y

newtype Max a = Max { getMax :: Maybe a } deriving (Eq, Ord, Show)
newtype Min a = Min { getMin :: Maybe a } deriving (Eq, Ord, Show)

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

  m `mappend` Max Nothing = m
  Max Nothing `mappend` n = n
  (Max m@(Just x)) `mappend` (Max n@(Just y))
    | x >= y = Max m
    | otherwise = Max n

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing

  m `mappend` Min Nothing = m
  Min Nothing `mappend` n = n
  (Min m@(Just x)) `mappend` (Min n@(Just y))
    | x <= y = Min m
    | otherwise = Min n


v = [(1,2),(3,4),(7,8)]


meh xs = bimap minimum maximum $ unzip xs :: (Int, Int)

{-| Smart element insertion

Gives back tuple of lists where element was inserted at a "smart" location

Examples:

>>> insertElem ([], [])    2
([2],[])
>>> insertElem ([], [])    2
([2],[])
>>> insertElem ([], [1])   2
([2,1],[])
>>> insertElem ([], [2])   2
([2,2],[])
>>> insertElem ([], [1,1]) 2
([2,1],[1])
>>> insertElem ([], [1,2]) 2
([2,1],[2])
>>> insertElem ([], [2,1]) 2
([2,2,1],[])
>>> insertElem ([], [2,2]) 2
([2,2,2],[])

>>> insertElem ([], [1,1,1]) 2
([2,1],[1,1])
>>> insertElem ([], [1,2,1]) 2
([2,1],[2,1])
>>> insertElem ([], [2,1,1]) 2
([2,2,1],[1])
>>> insertElem ([], [2,2,1]) 2
([2,2,2,1],[])
>>> insertElem ([], [1,1,2]) 2
([2,1],[1,2])
>>> insertElem ([], [1,2,2]) 2
([2,1],[2,2])
>>> insertElem ([], [2,1,2]) 2
([2,2,1],[2])
>>> insertElem ([], [2,2,2]) 2
([2,2,2,2],[])
-}
insertElem :: Eq a => ([a], [a]) -> a -> ([a], [a])
insertElem (a, []       ) el =            (a ++ [el]        , [])
insertElem (a, [b]      ) el =            (a ++ [el, b]     , [])
insertElem (a, (b:b2:bs)) el
  -- If the first element is not the same then it's a safe guard for the left-hand list
  | b /= el                  =            (a ++ [el, b]     , b2 : bs)
  -- back was the same as elem, move list 1 to the left and try again
  | b == el && b2 == el      = insertElem (a ++ [b]         , b2 : bs) el
  -- put new elem at the front
  | b == el && b2 /= el      =            (a ++ [el, b, b2] , bs)
  | otherwise                = error "This should never happen"


-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.

----------------------------------------------
-- | 'zipWith' generalises 'zip' by zipping with the function given
-- as the first argument, instead of a tupling function.
-- For example, @'zipWith' (+)@ is applied to two lists to produce the
-- list of corresponding sums.
--
-- 'zipWith' is right-lazy:
--
-- > zipWith f [] _|_ = []
{-# NOINLINE [1] zipWith' #-}
zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' _f []     _bs    = []
zipWith' _f _as    []     = []
zipWith' f  (a:as) (b:bs) = f a b : zipWith' f as bs






--

foo :: IO ()
foo = do
  getChar >>= \x ->
    print x

-- | Lazy ByteString version of chunksOf
chunksOf :: Int64 -> BL.ByteString -> [BL.ByteString]
chunksOf k = go
  where
    go t = case BL.splitAt k t of
             (a,b) | BL.null a    -> []
                   | otherwise    -> a : go b

-- | Lazy ByteString version of chunksOf
chunksOf' :: Int64 -> BL.ByteString -> [BL.ByteString]
chunksOf' k bs = let (a,b) = BL.splitAt k bs
                 in if BL.null b then [] else a : chunksOf' k b

-- ) [('A','B'),('C','D'),('B','A'),('E','F'),('F','E')]
beh xs =  [ x | x <- xs, y <- tail xs, fst x /= fst y, snd x /= snd y]


-- exercise 3.1


server = "irc.freenode.org"
port   = 6667

main = do
    g <- getNumCapabilities

    print g
