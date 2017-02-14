{-# LANGUAGE TemplateHaskell #-}

module Bed.Compose.Compose where

import Bed.Compose.ComposeTH


foo1 = $(composeN 1) -- (.)
foo2 = $(composeN 2) -- (.) . (.)
foo3 = $(composeN 3) -- (.) . (.) . (.)
