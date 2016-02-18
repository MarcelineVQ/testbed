{-# LANGUAGE NoImplicitPrelude #-}

module Do where

import Prelude(Char,Eq,Ord,IO,Show(..),(.), error, undefined, flip, const)
import qualified Prelude as P
import qualified System.Environment as E

type Chars = List Char

data List t = Nil | t :. List t deriving (Eq, Ord)

class Functor f where
  (<$>) :: (a -> b) -> f a -> f b
infixl 4 <$>

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  (=<<) :: (a -> f b) -> f a -> f b
infixr 1 =<<

(*>) :: Applicative f => f a -> f b -> f b
l *> r  = flip const <$> l <*> r

(>>=) = flip (=<<)

return :: Applicative f => a -> f a
return = pure

fail :: Applicative f => Chars -> f a
fail = error . hlist

(>>) :: Applicative f => f a -> f b -> f b
(>>) = (*>)

--(<*>) :: Monad f => f (a -> b) -> f a -> f b
--(<*>) p fa = (<$> fa) =<< p -- =<< fa

infixl 4 <*>

instance Monad IO where
  (=<<) =
    (P.=<<)

instance Monad [] where
  (=<<) =
    (P.=<<)

instance Applicative IO where
  pure =
    P.return
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

instance Applicative [] where
  pure =
    P.return
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

instance Functor IO where
  (<$>) =
    P.fmap

instance Functor [] where
  (<$>) =
    P.fmap

instance Show t => Show (List t) where
  show = show . foldRight (:) []

hlist :: List a -> [a]
hlist = foldRight (:) []

foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)

listh :: [a] -> List a
listh = P.foldr (:.) Nil

getArgs :: IO (List Chars)
getArgs = P.fmap (listh . P.fmap listh) E.getArgs

main :: IO ()
main2 :: IO ()
main3 :: IO ()

--2 and 3 fail in NICTA, why?
main = getArgs >>= \(d :. _) -> P.print d P.>> return ()
main2 = do { (d :. _) <- getArgs; P.print d P.>> return () }
main3 = do
  (d :. _) <- getArgs
  P.print d
  return ()
