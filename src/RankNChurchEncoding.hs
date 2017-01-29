{-# LANGUAGE RankNTypes #-}
module RankNChurchEncoding where

-- Attempting the exercise at the bottom of the Church Encoding section on
-- https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html

newtype ListC a =
    ListC {
      foldC :: forall r. (a -> r -> r) -> r -> r
    }

foldC' :: (a -> r -> r) -> r -> ListC a -> r
foldC' co ni (ListC f) = f co ni

nilC :: ListC a
nilC = ListC (\_ ni -> ni)

consC :: a -> ListC a -> ListC a
consC x (ListC f) = ListC (\co ni -> co x (f co ni))

instance Functor ListC where
    fmap f = foldC' (\x xs -> consC (f x) xs) nilC

unconsC :: (a -> ListC a -> r) -> r -> ListC a -> r
unconsC co ni l@(ListC f) = co (f _) l -- f (\x fconi -> co x (ListC (\co' ni' -> _))) ni

asList = unconsC (\x f' -> x : (asList f')) []
