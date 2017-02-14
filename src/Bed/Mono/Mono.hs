{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Bed.Mono.Mono where

import Bed.Mono.MonoTH
import Control.Monad.Identity

import Data.MonoTraversable

data Phoenician = ALEP | BET

data Gesh = Gesh

newtype Nak = Hup (Identity Gesh)

$(deriveMonoFunctor ''Nak)
