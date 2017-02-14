{-# LANGUAGE QuasiQuotes #-}

module Bed.TriangleGame where

import Text.RawString.QQ
import Data.List
import Control.Monad

-- In the triangle game you start off with six triangles numbered on each edge,
-- as in the example above. You can slide and rotate the triangles so they form
-- a hexagon, but the hexagon is only legal if edges common to two triangles
-- have the same number on them. You may not flip any triangle over. Two legal
-- hexagons formed from the six triangles are illustrated below.

-- The score for a legal hexagon is the sum of the numbers on the outside six
-- edges. Your problem is to find the highest score that can be achieved with
-- any six particular triangles.


triangleGroups = fmap (map words) $ triangleGrouper (drop 1 $ lines sampleInput)

triangleGrouper = filter ((==6) . length) . groupBy (\x y -> y /= "*" && x /= "*" && y /="$")



-- every configuration of each piece in set 1
rotTri = map rotates $ head triangleGroups

type Points = [String]
type Rotations = [Points]
data Triangle = Triangle Points Rotations

toDigits' :: Integer -> [Integer]
toDigits' 0 = [0]
toDigits' x = go x
  where
    go x
      | x <= 0 = []
      | otherwise = go (fromInteger x `div` 10) ++ [x `mod` 10]


toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    |otherwise = toDigits (fromInteger x `div` 10) ++ [x `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther [x, y] = [x, y*2]
doubleEveryOther (x:y:xs) = x : y*2 : doubleEveryOther xs

sumEm :: [Integer] -> Integer
sumEm [] = 0
sumEm [x] = 0
sumEm (x:xs) = sumEm (toDigits x) + sumEm xs

validate :: Integer -> Bool
validate x =
    let y = (sumEm . reverse . doubleEveryOther . reverse . toDigits) x
    in y `mod` 10 == 0


solver = do
  t <- triangleGroups
  triangles <- t





  return t


-- spin the triangle, return the spins
rotates = map (take 3) . take 3 . iterate (drop 1) . cycle


showScore s = if s == 0 then "none" else show s

sampleOutput = [r|
152
21
none|]

sampleInput = [r|
1 4 20
3 1 5
50 2 3
5 2 7
7 5 20
4 7 50
*
10 1 20
20 2 30
30 3 40
40 4 50
50 5 60
60 6 10
*
10 1 20
20 2 30
30 3 40
40 4 50
50 5 60
10 6 60
$|]
