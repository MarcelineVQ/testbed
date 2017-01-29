-- {-# LANGUAGE ScopedTypeVariables #-}

module Comonadic where

-- import Data.Functor.Fixedpoint

data Product e a = Product e a
class Functor w => Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  duplicate :: w a -> w (w a)
  duplicate = extend id

instance Functor (Product e) where
  fmap f (Product e a) = Product e (f a)

instance Comonad (Product e) where
  extract (Product e a) = a
  extend f (Product e a) = Product e (f (Product e a)) -- fmap f . duplicate
  duplicate (Product e a) = Product e (Product e a)

newtype Fix f = Fix { unFix :: f (Fix f) }

data Swayal f g d = Swayali (Fix g)

data Feh f g = Feh f

instance Functor (Swayal f g) where
  fmap f (Swayali g) = undefined


  --extend
  -- duplicate
