{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "something_which" doesnt_exist :: IO ()
something :: IO ()
something = doesnt_exist

main = something

class Pair a where
  pFirst :: Pair a => a -> a
  pSecond :: Pair a => a -> a
  ($$) :: (a -> b) -> Pair a -> Pair b

instance Pair (a, a) where
  pFirst (f, _) = f
  pSecond (_, s) = s
  ($$) fn (f, s) = (fn f, fn s)
