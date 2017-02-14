{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Bed.PP where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Text.PrettyPrint.GenericPretty

-- https://hackage.haskell.org/package/GenericPretty-1.2.1/docs/Text-PrettyPrint-GenericPretty.html
-- https://hackage.haskell.org/package/haskell-src-1.0.2.0/docs/Language-Haskell-Parser.html
deriving instance Generic (ParseResult HsModule)

str = "\
  \module Bed.Main where\n\
  \n\
  \main :: IO ()\n\
  \main = do\n\
  \  putStrLn \"hello world\"\n"

main :: IO ()
main = do
  pp $ parseModule str

{-
    No instance for (Out (ParseResult HsModule))
      arising from a use of ‘pp’
    In the expression: pp
    In a stmt of a 'do' block: pp $ parseModule str
    In the expression: do { pp $ parseModule str }

After adding  deriving instance Generic (Out (ParseResult HsModule))

    The first argument of ‘Generic’ should have kind ‘*’,
      but ‘Out (ParseResult HsModule)’ has kind ‘ghc-prim-0.4.0.0:GHC.Prim.Constraint’
    In the stand-alone deriving instance for
      ‘Generic (Out (ParseResult HsModule))’
-}
