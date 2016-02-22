{-# OPTIONS_GHC -fenable-rewrite-rules #-}

module Finger where

import Data.Foldable
import Data.Monoid
import Prelude hiding (head, tail)
import qualified Prelude as P

-- monad needs app and fmap because it can make them, if it couldn't make them
-- it wouldn't be a monad so a monad must also be an applicative and an fmap, if
-- it can't satisfy applicative and fmap it can't be a monad since a monad can
-- satisfy applicative and functor

-- fingertree bs
data FingerTree a = Empty
                  | Single a
                  | Deep (Digit a) (FingerTree (Node a)) (Digit a)
                  deriving Show
data Digit a = One a | Two a a | Three a a a | Four a a a a deriving Show
data Node a = Node2 a a | Node3 a a a deriving Show

-- how deep da tree?
--

-- Digit concat
-- Does not handle 4 digit addition, 4 is max
(+|+) :: Digit a -> Digit a -> Digit a
d1 +|+ d2 = toDigit $ toList d1 ++ toList d2

infixr 5 +|+

-- Does not handle over 4 digits, 4 is max
toDigit :: [a] -> Digit a
toDigit [a]       = One a
toDigit [a,b]     = Two a b
toDigit [a,b,c]   = Three a b c
toDigit [a,b,c,d] = Four a b c d
toDigit _        = error "Empty or > four digit."

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep (One a) Empty (One b)
a <| Deep (Four b c d e) m sf = Deep (Two a b) (Node3 c d e <| m) sf
a <| Deep pr m sf = Deep (One a +|+ pr) m sf

infixr 4 <|


(|>) :: FingerTree a -> a -> FingerTree a
Empty                    |> a = Single a
Single b                 |> a = Deep (One b) Empty (One a)
Deep pr m (Four e d c b) |> a = Deep pr (m |> Node3 e d c) (Two b a)
Deep pr m sf             |> a = Deep pr m (sf +|+ One a)

infixl 4 |>

toTree :: Foldable f => f a -> FingerTree a
toTree = foldr (<|) Empty

instance Foldable Digit where
  foldMap f (One a) = f a
  foldMap f (Two a1 a2) = f a1 <> f a2
  foldMap f (Three a1 a2 a3) = f a1 <> f a2 <> f a3
  foldMap f (Four a1 a2 a3 a4) = f a1 <> f a2 <> f a3 <> f a4

instance Foldable Node where
  foldMap f (Node2 a1 a2) = f a1 <> f a2
  foldMap f (Node3 a1 a2 a3) = f a1 <> f a2 <> f a3

instance Foldable FingerTree where
  foldMap f Empty = mempty
  foldMap f (Single x) = f x
  foldMap f (Deep d1 n d2) = foldMap f d1 <> foldMap (foldMap f) n <> foldMap f d2

--
-- data View s a = Nil | Cons a (s a)
--
-- view :: FingerTree a -> View FingerTree a
-- view Empty = Nil
-- view (Single x) = Cons x Empty
-- view (Deep pr m sf) = Cons (head pr) (deep (tail pr) m sf)
--
-- deep :: Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
-- deep (One a) m sf = case view m of
--   Nil -> toTree sf
--   Cons a m' -> Deep (One a) m' sf
-- deep pr m sf = Deep pr m sf
--
--
-- head :: Digit a -> a
-- head = P.head . toList
--
-- tail :: Digit a -> [a]
-- tail = P.tail . toList
--
























--
