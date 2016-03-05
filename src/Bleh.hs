{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Bleh where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Int
import Data.List

import Data.Char
import Data.Tuple.Extra
import Data.Function
import Data.Bifunctor
import Graphics.Gloss
import Data.Data
import Codec.Picture
import Data.Maybe
import Data.Bool

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

data Test = Test Int deriving (Show)

instance Num Test where
    (+) x y = x + y

data Muffin

zerp :: (a -> Bool) -> [a] -> [b] -> [(a,b)]
zerp _ _ [] = []
zerp _ [] _ = []
zerp p' xs' (y':ys') = zarp p' xs' ys' y'
  where
    zarp _ _ [] _ = []
    zarp _ [] _ _ = []
    zarp p (x:xs) (y:ys) b
      | p x = (x,y) : (zarp p xs ys y)
      | otherwise = (x,b) : zarp p xs (y:ys) b

stars :: String -> String
stars = unwords . map
  (\(x:xs) -> bool (x : map (\y -> if isAlpha y then '*' else y) (init xs) ++ [last xs]) (x:xs) (null xs))
  . words


-- Create a data structure that captures the phone layout
-- above. The data structure should be able to express
-- enough of how the layout works that you can use it to
-- dictate the behavior of the functions in the following
-- exercises

data DaPhone = DaPhone  [(String,String)] deriving Show

buttons = DaPhone  [ ("1", "1")
                   , ("2", "abc2")
                   , ("3", "def3")
                   , ("4", "ghi4")
                   , ("5", "jkl5")
                   , ("6", "mno6")
                   , ("7", "pqrs7")
                   , ("8", "tuv8")
                   , ("9", "wxyz9")
                   , ("0", " 0")
                   , ("#", ",.")
                   ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"]

-- Convert the following conversations into the keypresses
-- required to express them. We’re going to suggest types
-- and functions to fill in order to accomplish the goal,
-- but they’re not obligatory. If you want to do it differently…you do you

reverseTaps :: Char -> DaPhone -> [(String, Integer)]
reverseTaps ch (DaPhone ((x,y):zs)) = case elemIndex ch y of
                                       Just pos -> [(x, fromIntegral pos + 1)]
                                       Nothing  -> reverseTaps ch (DaPhone zs)

reverseTaps' :: DaPhone -> Char -> [(String, Integer)]
reverseTaps' (DaPhone ((x,y):zs)) ch = case elemIndex ch y of
                                       Just pos -> [(x, fromIntegral pos + 1)]
                                       Nothing  -> reverseTaps ch (DaPhone zs)

convertSentence :: String -> DaPhone -> [(String, Integer)]
convertSentence []  list = []
convertSentence (x:xs) list  = if isDigit x || isLower x  || x == ' '|| x == ',' || x == '.' then reverseTaps x list ++ convertSentence xs list else ("*", 1)  : reverseTaps (toLower x) list ++ convertSentence xs list


convertSentence' :: DaPhone -> String -> [(String, Integer)]
convertSentence' d = concatMap (\x ->
  if isUpper x then ("*", 1)  : reverseTaps (toLower x) d else reverseTaps x d)

-- this is pushing it a bit
convertSentence'' :: DaPhone -> String -> [(String, Integer)]
convertSentence'' d = concatMap $ bool id (("*", 1) :) . isUpper <*> reverseTaps' d

-- this is pushing it way too much
-- the list monad concats for us you see, clever , but opaque as fuck
convertSen :: DaPhone -> String -> [(String, Integer)]
convertSen = (=<<) . ((bool id (("*", 1) :) . isUpper) <*>) . reverseTaps'


convertCo :: [String] -> DaPhone -> [(String, Integer)]
convertCo [] list = []
convertCo (x:xs) list = convertSentence x list ++ convertCo xs list

-- unscanl (-) [100,99,98,97,96] == [1,1,1,1,1]

-- unscanl :: (a -> b -> b) -> [a] -> [b]
-- unscanl f = unfoldr f

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish . map f $ xs

-- splitAt' [2,3,4] ("helloworld" :: [Char]) == ["he", "llo", "worl", "d"]
splitAt' :: [Int] -> [Char] -> [[Char]]
splitAt' _ [] = []
splitAt' [] xs = [xs]
splitAt' (x:xs) ys = take x ys : splitAt' xs (drop x ys)

-- import Data.List

histogram :: [Integer] -> String
histogram a = draw $ map (`take` (repeat '*')) (map length $ group $ sort a)
  where
    draw ls = show ls


capitalizeWord :: String -> (String, String)
capitalizeWord [] = ([],[])
capitalizeWord w@(x:xs) = (w, toUpper x : map toLower xs)
--
-- capitalizeWord :: String -> (String, String)
-- capitalizeWord w
--    | null w = (o,w)
--    | otherwise = (o, toUpper x : map toLower xs)
--    where o@(x:xs) = w

capitalizeWords :: String -> [(String, String)]
capitalizeWords w = map capitalizeWord (words w)


--
-- break                   :: (a -> Bool) -> [a] -> ([a],[a])
-- break _ xs@[]           =  (xs, xs)
-- break p xs@(x:xs')
--            | p x        =  ([],xs)
--            | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)

feh :: (a -> Bool) -> Int -> [a] -> [(a,Int)]
feh _ _ [] = []
feh f n (x:xs) = if f x then (x,n+1) : feh f (n+1) xs
                 else (x,n) : feh f n xs


zipStep :: forall a b . (a -> Bool) -> [a] -> [b] -> [(a,b)]
zipStep f xs as@(y:ys) = zipWith (go y) xs as
  where
    go :: b -> a -> b -> (a, b)
    -- go _ x y = (x, y)
    go _ x y = if f x then (x, y) else (x, y) -- WTF!


roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show


beffh = foldr f [] [1,3,12,13,4,3,3,22,4,22,9,22,10]
  where
    f x [] = [x]
    f x acc@(a:_) = case compare x a of
      GT -> [x]
      EQ -> x : acc
      LT -> acc

beffh' = uncurry replicate $ foldr1 f $ zip [1,1..] [1,3,12,13,4,3,3,22,4,22,9,22,10]
  where
    f (n',x) (n,a) = case compare x a of
      GT -> (n',x)
      EQ -> (n+1,a)
      LT -> (n,a)

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
