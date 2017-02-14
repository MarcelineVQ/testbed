{-# LANGUAGE TemplateHaskell #-}

module Bed.Compose.ComposeTH where

import Language.Haskell.TH


composeN :: Int -> Q Exp
composeN n = let c = [| (.) |]
                 v = replicate (n-1) c
              in foldr (`infixApp`c) c v
