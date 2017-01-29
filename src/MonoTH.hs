{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MonoTH where

import Language.Haskell.TH
import Data.MonoTraversable

import Language.Haskell.TH.Syntax
import Data.Function


{- bollu paste

-- Assume Func is a functor
-- Mono is a monomorphised functor

data A = ...
newtype Mono = MonoCons (Func A)

$(deriveMonoFunctor Mono)

{-
this should automatically emit:
-}
-- omap :: (A -> A) -> Mono -> Mono

type instance (Element Mono) = A

instance (MonoFunctor Mono) where
  omap f (MonoCons fa) = MonoCons (fmap f fa)

-}


-- TH attempt, usage:
-- $(deriveMonoFunctor ''TypeToDerive)

-- This is setup to only work for newtype wrapping a functor value, like the example
-- to make it work for data as well, add a case expression instead of just NewtypeD

deriveMonoFunctor :: Name -> DecsQ
deriveMonoFunctor t = do
  (TyConI tyCon) <- reify t -- access compile-time information about our type

  let (NewtypeD _ mono _ _ con _) = tyCon -- get newtype name and constructor
      (NormalC monocons ((_,funcA):_)) = con -- get wrapped constructor name and value type
      (AppT func a) = funcA -- func is our functor type, a is our value type
      v = pure a -- convenience
      -- We now have the names we need to build the rest

      -- Our type instance declaration
  let typeInstance = tySynInstD ''Element (tySynEqn [conT mono] v)

      -- Out MonoFunctor declaration, messy though, there's probbaly a cleaner
      -- way to do the patterns that I don't know
      functorInstance = instanceD (cxt []) (appT (conT ''MonoFunctor) (conT mono))
        [funD (mkName "omap") [clause [[p|f|], conP monocons [[p|fa|]]]
        (normalB (appE (conE monocons) [e|(fmap f fa) |])) []]]

  sequence [typeInstance, functorInstance]
