{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "something_which" doesnt_exist :: IO ()
something :: IO ()
something = doesnt_exist

main = something
