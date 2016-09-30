--{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-Haskell implementation of a simple ceasarcipher-}
module Test where

import Network
import System.IO

import Text.Parsec
import Text.Parsec.Combinator
import Data.List
import Control.Monad.State.Lazy
import Data.Monoid hiding ((<>))
import Data.Semigroup
import qualified Test.QuickCheck as Q
import Control.Applicative
import Data.Foldable

import Prelude
import qualified Prelude as LT

data RoseTree a = RoseTree a [RoseTree a] deriving (Show)

instance Functor RoseTree where
  fmap f (RoseTree a as) = RoseTree (f a) (fmap f <$> as)

instance Applicative RoseTree where
  pure a = RoseTree a []
  RoseTree f tfs <*> tx@(RoseTree x txs) = RoseTree (f x) (fmap (f <$>) txs ++ fmap (<*> tx) tfs)

instance Monad RoseTree where
  return = pure
  --(>>=) :: RoseTree a -> (a -> RoseTree b) -> RoseTree b
  (>>=) = bindTree


fw x = x + x

doubleSecond  xs   = doubleSecond' xs True
doubleSecond' [] _ = []
doubleSecond'  (x:xs)  a
 | a==True   =(2*x):doubleSecond' xs False
 | otherwise = x:doubleSecond' xs True

replicompose 5 f = f . f . f . f . f

replicompose' :: Int -> (a -> a) -> a -> a
replicompose' n f = foldr (.) id (replicate n f)

rebot n f = iterate (f .) id !! n

--fixter p xs = fix (\f)

iterfix :: (Num b, Eq b) => b -> (a -> a) -> (a -> a)
iterfix = fix (\rec' n f -> if n == 0 then id else f . rec' (n-1) f)

bleeh :: Int -> (a -> a) -> a -> a
bleeh n f = appEndo . foldMap Endo $ replicate n f

bleeh' :: (Monoid a) => Int -> (a -> a) -> a -> a
bleeh' n f = fold $ replicate n f

-- bleueh = repliblah 5 head

--
repliFix :: (Num b, Eq b) => b -> (a -> a) -> (a -> a)
repliFix = fix (\rec' n f -> if n == 0 then id else f . rec' (n-1) f)

chunksOf :: Int -> [a] -> [[a]]
chunksOf = fix (\f acc n xs -> if null xs then acc ++ [] else f (acc ++ [take n xs]) n (drop n xs)) []
-- chunksOf 4 [1,2,3,4,5,6,7,8,9,10]


main = print $ rebot 500000 (*2) 2

data Tr a = Lf | Nd (Tr a) a (Tr a)
          deriving Show

insert' :: Int -> Tr Int -> Tr Int
insert' n Lf = Nd Lf n Lf
insert' n (Nd lTr tn rTr)
  | n < tn = Nd (insert' n lTr) tn rTr
  | n > tn = Nd lTr tn (insert' n rTr)


rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
  | m > n  = 0
  | n == 0 = 1
  | otherwise = m * rangeProduct m (n - 1)

toRevDigits :: Integer -> [Integer]
toRevDigits n
   | n <= 0    = []
   | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = (x `div` 10) + (x `mod` 10)
sumDigits (x:xs) = sumDigits [x] + sumDigits xs


multiplication :: Integer -> Integer -> Integer
multiplication  x y
         | x < 0 || y < 0    = error "multiplication works only with numbers greater then 0"
         | y == 0 || y == 0  = 0
         | otherwise         = x + multiplication x (y - 1)





-- exercise 20

-- The integer square root of a positive integer n is the largest integer whose square
-- is less than or equal to n. For instance, the integer square roots of 15 and 16 are
-- 3 and 4, respectively. Give a primitive recursive definition of this function.



{-

zipWith (*) <*> tail $ [1..10]
f (a -> b) -> f a -> f b

t -> ( a -> b) -> (t -> a) -> (t -> b)
(<*>) :: ((->) t (a -> b)) -> ((->) t a) -> ((->) t b)

zipWith (*) = [a] -> [a] -> [a]
tail = [a] -> [a]

(<*>) f1 f2 f3 = f1 f3 (f2 f3)

(->) (<*>) f g x = f x ( g x)




-}

orderTriple (a,b,c) = let [x,y,z] = sort [a,b,c] in (x,y,z)


{-
sorted (x:y:xs)
  | x <= y = ascending (y:xs)
  | otherwise = descending (y:xs)
  where
    ascending as (x:xs)
      | x < = y = x : sorted xs
      | otherwise = descending xs

    descending ds (x:xs) = sorted (xs ++ [x])

    collect
sorted xs = xs
-}


beh' = zipWith (*) <*> tail $ [1..10]


{-
rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
  | m > n  = 0
  | otherwise = rangeProduct2 m n
  where
  rangeProduct2 :: Integer -> Integer -> Integer
  rangeProduct2 m n
      | m > n    = 1
      | m <= n   = m * rangeProduct2 (m + 1) n

plusAndDouble x = double (x+1)
  where
  double x = x*x

-}
bindTree :: RoseTree a -> (a -> RoseTree b) -> RoseTree b
bindTree t@(RoseTree x xs) f = undefined --pure f <$> x <*> xs

map' :: (a -> b) -> [a] -> [b]
map' = map

server = "irc.freenode.org"
port   = 6667

main' = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    t <- hGetContents h
    print t
h :: (f a -> f b)
h = undefined

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs x y z = (maxNum, length (filter (== maxNum) els))
    where maxNum = maximum els
          els = [x, y, z]

--maxThreeOccurs' x y z
maxOccurs :: (Num a, Ord a) => [a] -> (a, a)
maxOccurs xs =
  let maxNum = maximum xs
  in (maxNum, foldl' (\acc x -> if x == maxNum then acc+1 else acc) 0 xs)

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z
   | x >= y && x >= z = x
   | y >= z           = y
   | otherwise        = z

maxFour3 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour3  a b c  = max(maxThree a b c)

hylo :: Functor f => (f b -> b) -> (a -> f a) -> (a -> b)
hylo f g = f . fmap (hylo f g) . g --fa -> fb

less' (_, []) = []; less' (e, x:xs) = if x < e then x : less' (e, xs) else less' (e, xs)

less (_, []) = []
less (e, x:xs)
  | x < e = x : less (e, xs)
  | otherwise = less (e, xs)

intclist _ [] = []
intclist n (x:xs) = x+n : intclist n xs


-- | Simple tree data type
data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show)

-- | Insertion method for the tree. Duplicates are ignored.
insertTree :: Ord a => a -> Tree a -> Tree a
insertTree i Leaf = Node Leaf i Leaf
insertTree i (Node left a right) = case compare i a of
    EQ -> Node left a right
    LT -> Node (insertTree i left) a right
    GT -> Node left a (insertTree i right)


{-
filterTree :: (a -> Bool) -> Tree a -> Tree a
filterTree p Leaf = Leaf
filterTree p t@(Node Leaf a Leaf) = if p a
    then t
    else Leaf
filterTree p (Node Leaf a right) = if p a
    then Node Leaf a (filterTree p right)
    else filterTree p right
filterTree p (Node left a Leaf) = if p a
    then Node Leaf a (filterTree p left)
    else filterTree p left
filterTree p (Node left a right) = if p a
    then Node (filterTree p left) a (filterTree p right)
    else case (leftFilter,rightFilter) of
            (Leaf, Leaf) -> Leaf
            (Leaf, right) -> right
            (left, Leaf) -> left
            (left, right) -> case yankTree left of
                Nothing -> right -- shouldn't happen, but it might?
                Just (subVal,subRest) -> Node subRest subVal right
         where leftFilter = filterTree p left
               rightFilter = filterTree p right
-}

filterTree' :: (a -> Bool) -> Tree a -> Tree a
filterTree' p = craft . filter p . collectTree
  where
  craft :: [a] -> Tree a
  craft = foldr simpleInsert Leaf

  simpleInsert :: a -> Tree a -> Tree a
  simpleInsert a Leaf = Node Leaf a Leaf
  simpleInsert a (Node Leaf v right) = Node (simpleInsert a Leaf) v right
  simpleInsert a (Node left v Leaf) = Node left v (simpleInsert a Leaf)
  simpleInsert a (Node left v right) = Node (simpleInsert a left) v right

  collectTree :: Tree a -> [a]
  collectTree Leaf = []
  collectTree (Node Leaf a right) = [a] ++ collectTree right
  collectTree (Node left a Leaf) = collectTree left ++ [a]
  collectTree (Node left a right) = collectTree left ++ [a] ++ collectTree right


foo = ((+1) LT.. (+2)) 2

-- import Control.Monad.State.Lazy
-- from mtl, https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html#t:StateT


lastDigit :: Integer -> Integer
lastDigit x = read [last . show $ x]


stringToInteger :: String -> Integer
stringToInteger x = read x

charToInteger :: Char -> Integer
charToInteger x = read [x]

integerToString :: Integer -> String
integerToString x = show x


digits :: Integer -> [Integer]
digits = map (read . flip (:)[]) . show


dropLastDigit :: Integer -> Integer
dropLastDigit x
 | x`div`10 == 0 = 0
 | otherwise = x`div`10

dropFirstDigit :: Integer -> Integer
dropFirstDigit = read . drop 1 . show

avrg :: (Fractional a, Foldable t) => t a -> a
avrg x = sum x / (fromIntegral . length) x

flatten :: [[a]] -> [a]
flatten = concat

flatmap f = flatten . map f
flatmap' :: (a -> [b]) -> [a] -> [b]
flatmap' = (flatten .) . map

breakString :: Char -> String -> String
breakString _ [] = []
breakString c (x:xs)
  | x == c = xs
  | otherwise = breakString c xs


foldl_5                :: (b -> a -> b) -> b -> [a] -> b
foldl_5 f z xs          = foldr (\x rebc a -> rebc (f a x)) (\a -> a) xs z




newtype Observer r a = Observer {runObserve :: r -> a}

instance Monoid a => Monoid (Observer r a) where
  mappend (Observer f) (Observer g) = Observer (\x -> f x `mappend` g x)
  mempty = Observer (const mempty)

newtype Mem s a = Mem {runMem :: s -> (a,s)}

instance Monoid a => Monoid (Mem s a) where
  --mappend (Mem f) (Mem g) = Mem (\x -> (fst (f x) `mappend` fst (g x), x))
  mappend (Mem f) (Mem g) = Mem (\x -> let (x',s') = f x
                                           (x'', s'') = g s'
                                       in (x' `mappend` x'', s''))
  mempty = Mem (\x -> (mempty, x))

beh = do
  let f = Observer (\x -> Sum (x + 1))
      g = Observer (\x -> Sum (x + 2))
  print $ runObserve (f `mappend` mempty) 3
  let f = Mem (\x -> (Sum x + 1, x + 1))
      g = Mem (\x -> (Sum x - 1, x - 1))
  print $ runMem (f `mappend` mempty) 0

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

-- Results should be:
-- let f = Combine $ \n -> Sum (n + 1)
-- let g = Combine $ \n -> Sum (n - 1)
-- unCombine (f <> g) $ 0 --> Sum 0
-- unCombine (f <> g) $ 1 --> Sum 2
-- unCombine (f <> f) $ 1 --> Sum 4
-- unCombine (g <> f) $ 1 --> Sum 2

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) f g = Combine $ \x -> (unCombine f $ x) <> (unCombine g $ x)


prop_Associative :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
prop_Associative f a b c = f a ( f b c ) == f (f a b) c

type CombineSum = Combine Int (Sum Int)
combineSmush :: CombineSum -> CombineSum -> CombineSum
combineSmush = (<>)

--f <$> a = f <*> pure a
co::(b->c)->(a->b)->(a->c)
co' = (.)

co = \f g x -> f (g x)

average :: [Int] -> Int
average list = sum list `div` length list

aboveAverage :: [Int] -> Int
aboveAverage list = length(filter (> average list) list)


data Move = Rock |
            Paper |
            Scissors
            deriving (Show,Eq)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper

data Result = Lose |
              Win   |
              Draw
              deriving (Show,Eq)
outcome :: Move -> Move -> Result
outcome m1 m2
  | beat m2 == m1 = Win
  | m2 == m1 = Draw
  | otherwise = Lose

--prop_beat_lost :: Move -> Bool
--prop_beat_lost = (id ==) . beat . lose

prop_win x y = beat x == y Q.==> outcome x y == Win
--foldl1' = foldl1

--import Data.List (foldl1')

-- Factorial function
fact n = foldl1' (*) [1..n]

-- Permutation function
n `p` r = foldl1' (*) [n-r+1..n]

-- Main function
unique :: Integer -> Integer -> Integer
unique c n = c * foldl1' (\acc x -> acc +
        foldl1' (\acc h ->
            let npxh = n `p` (x-h)
                threeph = 3 `p` h
            in acc +
                fact(npxh + threeph) * 26 ^ h
                `div` (fact npxh * fact threeph)
        ) [0..3]
    ) [3..12]

--main = print $ unique 6 45
--main'' = print $ unique 2 3


--
