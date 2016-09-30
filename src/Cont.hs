
module Cont where

import Control.Monad.Cont


ex1 = do
  a <- return 1
  b <- return 10
  return $ a + b

test1 = runCont ex1 show

ex2 = do
  a <- return 1
  b <- ContT $ \fred -> fred 10
  return $ a + b

test2 = runCont ex2 show

ex3 = do
 a <- return 1
 b <- ContT (\fred -> "escape")
 return $ a+b

test3 = runContT ex3 show



ex4 = do
  a <- return 1
  b <- ContT $ \fred -> fred 10 ++ fred 20
  return $ a + b

test4 = runContT ex4 show
