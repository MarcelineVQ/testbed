{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}


module Bed.What where

import Control.Lens

import Data.List
import Data.Monoid
import Data.Functor.Identity
import Data.Maybe

import Control.Applicative

--
-- pre :: (Monoid (f a), Applicative f) => (a -> Bool) -> a -> f a
-- pre p x = if p x then pure x else mempty
--
-- -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- -- foldMap :: (Monoid (f a)), Foldable t, Applicative f) => (a -> f a) -> t a -> f a
--
-- filterF :: (Monoid (f a), Foldable t, Applicative f) => (a -> Bool) -> t a -> f a
-- filterF p xs = foldMap (pre p) xs
--
-- beh = getSum $ filterF (>2) [1..10]

-- boo = appEndo $ filterF (>3) [1..10]

-- boo = getFirst $ filterF (>4) [Nothing, Just 3, Just 5, Just 7]

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst (g (f x))

mang :: (x -> y) -> (y -> (w, z)) -> x -> w
mang f g x = fst . g . f $ x

mong :: (x -> y) -> (y -> (w, z)) -> x -> w
mong = ((fst .) .) . flip (.)

mWhileJust :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
mWhileJust control action = do
  c <- control
  case c of
    Nothing -> return ()
    Just r -> do
      action r
      mWhileJust control action

str :: Int -> [Char]
str a = digits a []
 where digits value     | value >= 10 = digits (div value 10) . (chr (mod value 10) :)
                        | value >= 0 = (chr value :)
                        | otherwise = ('-' :) . digits (-value)
       chr = ("0123456789" !!)


data Tree a = Leaf a | Branch [Tree a] deriving (Ord, Eq, Show)

foo = [1,2,2,1,2,3,2]
-- Should become:
-- Branch [
--    Leaf 1,
--    Branch [
--        Leaf 2,
--        Leaf 2
--    ],
--    Leaf 1,
--    Branch [
--        Leaf 2,
--        Branch [
--            Leaf 3
--        ],
--        Leaf 2
--   ]
-- ]





--
-- test = Branch [   Leaf 1,   Branch [       Leaf 2,       Leaf 2   ],   Leaf 1,   Branch [       Leaf 2,       Branch [           Leaf 3       ],       Leaf 2  ]]
-- -- toTree :: (Ord a) => [a] -> Tree a -> Tree a
-- -- toTree []     t               = t
-- -- toTree (x:xs) (Branch [])     =
-- -- toTree (x:xs) (Leaf a)        = Branch [l]
-- -- toTree (x:xs) (Branch (b:bs)) =
--
-- foldTree :: (a -> b -> b) -> b -> Tree a -> Tree b
-- foldTree f b (Leaf a) = Leaf (f a b)
-- -- foldTree f b (Branch []) = Branch [Leaf b]
-- foldTree f b (Branch ts) = Branch (foldr (foldTree f b) [] ts)
--
-- walkFoo :: Int -> [Int] -> Tree Int
-- walkFoo nest (item:rest) =
--   case compare item nest of
--     GT -> Branch [walkFoo item rest]
--     EQ -> Leaf item
--     LT -> Leaf item
--
-- main :: IO ()
-- main = do
--   putStrLn $ show $ walkFoo 0 foo
--



-- foo = uncurry (foldr max) -- <$> uncons

-- fooo :: (a -> Bool) -> [a] -> [a]
-- fooo p = unfoldr (maybe Nothing (\x -> if p (fst x) then Just x else Nothing) . uncons)

-- import Data.List (intercalate)

show2 :: (Show a, Show b) => a -> b -> String
show2 a b = show (a,b)

moo = putStrLn . unwords $ show2 <$> [1,2,3] <*> [4,5,6]




{-
I get:
(1,4) (1,5) (1,6) (2,4) (2,5) (2,6) (3,4) (3,5) (3,6)

I want:
(1,4) (1,5) (1,6)
(2,4) (2,5) (2,6)
(3,4) (3,5) (3,6)
-}



{-

foo :: (a -> b) -> a -> b

When you see foo :: (a -> b) -> a -> b you already know a few things about foo

-> is the type constructor of functions so we know just by seeing -> that the
declaraton is a function, as a counter-example `bar :: a` isn't a function
You also know that the first argument (a -> b) is for sure a function because
(a -> b) has -> in it. We have (a -> b) and a, and we need to result in b. The
only way to get something different is to apply a function to it, so we look and see
-}
