module Cookie where

import Data.List
import Data.List.Split

list v fs = scanl (\x f -> f x) v


-- [65.187,423.71549999999996,1271.1464999999998]

million x = x *     1000000
billion x = x *     1000000000
trillion x = x *    1000000000000
quadrillion x = x * 1000000000000000
quintillion x = x * 1000000000000000000
sextillion x = x *  1000000000000000000000
septillion x = x *  1000000000000000000000000
octillion x = x *   1000000000000000000000000000

type CPS = Integer
type Ratio = Double

data Building = Building { bType :: Btype, cps :: CPS, ratio :: Ratio }
              deriving (Show, Eq, Ord)

data Btype = Cursor
           | Grandma
           | Farm
           | Mine
           | Factory
           | Bank
           | Temple
           | WizardTower
           | Shipment
           | AlchemyLab
           | Portal
           | TimeMachine
           | AntimatterC
           | Prism
           deriving (Show, Eq, Ord, Enum)

buildings =
          [ Building Cursor (trillion 1019) 0.0
          , Building Grandma (trillion 900) 0.0
          , Building Farm (362643000) 0.0
          , Building Mine (million 1330) 0.0
          , Building Factory (million 5883) 0.0
          , Building Bank (million 13853) 0.0
          , Building Temple (million 35270) 0.0
          , Building WizardTower (million 186471) 0.0
          , Building Shipment (billion 1049) 0.0
          , Building AlchemyLab (billion 6213) 0.0
          , Building Portal (billion 37650) 0.0
          , Building TimeMachine (billion 238577) 0.0
          , Building AntimatterC (trillion 1545) 0.0
          , Building Prism (trillion 10233) 0.0
          ]
multi :: Building -> Float
multi b = undefined

-- -- given a specific building cost, spit out the cost evrything else should be
-- compute :: Btype -> Double -> String
-- compute t c = let ratio = sortByCPS buildings
--
--               in format b

showBuildings :: Integer -> [Building] -> String
showBuildings base = unlines . map fun
  where
    fun (Building t c r) = intercalate " " [space 15 (show t), space 15 (format (truncate $ r * fromInteger base)), show r]
    space n s = let len = length s
                in if len < n
                     then s ++ replicate (n-len) ' '
                     else s

compute :: Btype -> Integer -> [Building] -> String
compute t n = showBuildings n . ratios' . dropWhile ((/= t) . bType) . sortByCPS

compute' :: Btype -> Integer -> [Building] -> String
compute' t n = showBuildings n . reverse . dropWhile ((/= t) . bType) . ratios . sortByCPS

format :: CPS -> String
format c = let num = map reverse . reverse . chunksOf 3 $ reverse (show c)
           in intercalate "." (take 2 num) ++ suffix !! (length num-3)

suffix = ["mi","bi","tri","qad","quin","sex","sep","oct", "non","dec"]

ratios :: [Building] -> [Building]
ratios b = let ra = foldr (\(o,n) t -> fromInteger (cps n) / fromInteger (cps o) : t) [] . (zip <*> tail) $ nb :: [Ratio]
               ra' = scanl (/) 1.0 ra
               nb = sortByCPS b
            in zipWith (\b' r -> b' { ratio = r} ) (reverse nb) ra'

ratios' :: [Building] -> [Building]
ratios' b = let ra = foldr (\(o,n) t -> fromInteger (cps n) / fromInteger (cps o) : t) [] . (zip <*> tail) $ nb :: [Ratio]
                ra' = scanl (*) 1.0 ra
                nb = sortByCPS b
            in zipWith (\b' r -> b' { ratio = r} ) nb ra'

sortByCPS :: [Building] -> [Building]
sortByCPS = sortOn cps
