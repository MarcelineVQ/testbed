{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}

module Bed.CD.D where

import Generics.MultiRec.Base
import Generics.MultiRec.TH
import Bed.CD.C

-- * Instantiating the library for AST using TH

-- ** Index type

data AST :: * -> * -> * where
  Expr  ::  AST a (Expr a)
  Decl  ::  AST a (Decl a)
  Var   ::  AST a (Var  a)
