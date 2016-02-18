

module Fingers where

import Data.Foldable
import Data.Monoid

-- monad needs app and fmap because it can make them, if it couldn't make them
-- it wouldn't be a monad so a monad must also be an applicative and an fmap, if
-- it can't satisfy applicative and fmap it can't be a monad since a monad can
-- satisfy applicative and functor

-- fingertree bs
data FingerTree a = Empty
                  | Single a
                  | Deep (Digit a) (FingerTree (Node a)) (Digit a)
data Digit a = One a | Two a a | Three a a a | Four a a a a
data Node a = Node2 a a | Node3 a a a
--
-- depth :: FingerTree a -> Int
-- depth = traverse deep
--
--
-- deep :: FingerTree a -> [Int] -> Int
-- deep (Single _) b
--   | b == 0 = 1
--   | otherwise = b+1
-- deep _ b = b

-- instance Foldable Node where
--   foldr f b (Node2 a a') = f a (f a' b)
--   foldr f b (Node3 a a' a'') = f a (f a' (f a'' b))

class Reduce f where
  reducer :: (a -> b -> b) -> (f a -> b -> b)
  reducel :: (b -> a -> b) -> (b -> f a -> b)

instance Reduce [] where
  reducer (-<) x z = foldr (-<) z x
  reducel (>-) x z = foldl (>-) x z

toList :: (Reduce f) => f a -> [a]
toList s = s =| [] where (=|) = reducer (:)

instance Foldable Digit where
  foldr f b (One a) = f a b
  foldr f b (Two a1 a2) = f a1 (f a2 b)
  foldr f b (Three a1 a2 a3) = f a1 (f a2 (f a3 b))
  foldr f b (Four a1 a2 a3 a4) = f a1 (f a2 (f a3 (f a4 b)))

instance Foldable Node where
  foldMap f (Node2 a1 a2) = f a1 <> f a2
  foldMap f (Node3 a1 a2 a3) = f a1 <> f a2 <> f a3

-- instance Foldable FingerTree where
--   foldr f b Empty = b
--   foldr f b (Single x) = f x b
--   foldr f b (Deep d1 n d2) = fald f d1 (fald (fald f) n (fald f d2 b))
--     where -- cut out flip on n
--       fald f xs b = foldr f b xs

instance Foldable FingerTree where
  foldMap f Empty = mempty
  foldMap f (Single x) = f x
  foldMap f (Deep d1 n d2) = foldMap f d1 <> foldMap (foldMap f) n <> foldMap f d2

instance Traversable FingerTree where
  traverse = undefined

instance Functor FingerTree where
  fmap f t = undefined
