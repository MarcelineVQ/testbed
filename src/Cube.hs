{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cube where

import Prelude hiding (all, and, flip, map, filter)

data R
data G
data B
data W

data Cube u f r b l d

type CubRed = Cube R R R R R R
type CubeBlue = Cube B B B B B B

type Cube1 = Cube B G W G B R
type Cube2 = Cube W G B W R R
type Cube3 = Cube G W R B R R
type Cube4 = Cube B R G G W W

class Transforms u f r b l d where
  rot :: Cube u f r b l d -> Cube u r b l f d
  twist :: Cube u f r b l d -> Cube f r u l d b
  flip :: Cube u f r b l d -> Cube d l b r f u

instance Transforms u f r b l d where
  rot = undefined
  twist = undefined
  flip = undefined

-----

data True
data False

class And b1 b2 b | b1 b2 -> b where
  and :: b1 -> b2 -> b

instance And True True True where and = undefined
instance And True False False where and = undefined
instance And False True False where and = undefined
instance And False False False where and = undefined

-----

data Nil
data Cons x xs -- not used, ::: is now 'Cons'

data x ::: xs
infixr 5 :::

data x :|: xs
infixl 5 :|:

class ListConcat l1 l2 l | l1 l2 -> l where
  listConcat :: l1 -> l2 -> l

instance ListConcat Nil l l where listConcat = undefined
instance (ListConcat xs ys zs) => ListConcat (x ::: xs) ys (x ::: zs) where
  listConcat = undefined

-----

class Apply f a b | f a -> b where apply :: f -> a -> b

data Rotation
data Twist
data Flip

instance Apply Rotation (Cube u f r b l d) (Cube u r b l f d)
  where apply = undefined

instance Apply Twist (Cube u f r b l d) (Cube f r u l d b)
  where apply = undefined

instance Apply Flip (Cube u f r b l d) (Cube d l b r f u)
  where apply = undefined

-----

class Map f xs zs | f xs -> zs where
  map :: f -> xs -> zs

instance Map f Nil Nil where map = undefined
instance (Apply f x z, Map f xs zs) => Map f (x ::: xs) (z ::: zs)
  where map = undefined

-----

class Filter f xs zs | f xs -> zs where
  filter :: f -> xs -> zs

instance Filter f Nil Nil where filter = undefined
instance (Apply f x b, Filter f xs ys, AppendIf b x ys zs) =>
  Filter f (x ::: xs) zs where filter = undefined

class AppendIf b x ys zs | b x ys -> zs
instance AppendIf True x ys (x ::: ys)
instance AppendIf False x ys ys

















-----


cubes = ["BGWGBR", "WGBWRR", "BRGGWW"]
u = undefined




--
