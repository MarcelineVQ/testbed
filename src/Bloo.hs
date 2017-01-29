module Bloo where

import Data.Bifunctor
import Data.Tuple
import Data.List (uncons)

import System.IO
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import System.FilePath




-- <jle`> is there a common (Maybe a, b) -> Maybe (a, b) in base?
-- <jle`> i know about sequence :: (a, Maybe b) -> Maybe (a, b)
-- * dmj__ is now known as dmj`
-- <jle`> but i wonder if there's something for the first

-- (a,b) (,b) <$> a



main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Number of elements which make up the association table.
    n <- readLn :: IO Int
    -- Number Q of file names to be analyzed.
    q <- readLn :: IO Int

    extLines <- replicateM n getLine
    inLines <- replicateM q getLine

    let extMap = M.fromList $ (\(ext:mime:_) -> (map toLower ext,mime)) . words <$> extLines
        outLines = map f inLines
        f ext = fromMaybe "UNKNOWN" (M.lookup (drop 1 . takeExtension . map toLower $ ext) extMap)
    mapM_ putStrLn outLines


class Bitraversable t where
  bitraverse     :: Applicative f => (a -> f b) -> (c -> f d) -> t a c -> f (t b d)

  bisequence :: Applicative f => t (f b) (f d) -> f (t b d)
  bisequence = bitraverse id id

  sequenceLeft  :: Applicative f => t (f b) c -> f (t b c)
  sequenceLeft = bitraverse id pure

  sequenceRight :: Applicative f => t a (f d) -> f (t a d)
  sequenceRight = bitraverse pure id

instance Bitraversable (,) where
  bitraverse f g (a,b) = (,) <$> f a <*> g b


sequenceLeft'' :: (Applicative f, Bitraversable t) => t (f a) b -> f (t a b)
sequenceLeft'' = bitraverse id pure -- first (\a -> _) x

type Stack a = [a]

push :: a -> Stack a -> Stack a
push = (:)

pop' :: Stack a -> (Maybe a, Stack a)
pop' s = maybe (Nothing,s) (first Just) . uncons $ s

pop :: Stack a -> (Maybe a, Stack a)
pop [] = (Nothing, [])
pop (x:xs) = (Just x, xs)

empty :: Stack a
empty = []

isEmpty :: Stack a -> Bool
isEmpty = Prelude.null

top :: Stack a -> Maybe a
top [] = Nothing
top (x:_) = Just x
