{-# LANGUAGE TemplateHaskell #-}

module Compose where

import ComposeTH


foo1 = $(composeN 1) -- (.)
foo2 = $(composeN 2) -- (.) . (.)
foo3 = $(composeN 3) -- (.) . (.) . (.)
