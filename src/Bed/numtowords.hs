import Data.List (sort, tails, group)
import Data.Char (toLower)

type Word = [Char]
type Text = [Char]

main :: IO ()
main = putStrLn $ convert 23053

units = ["zero","one","two","three","four", "five", "six","seven","eight","nine"]
teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
tens = ["twenty","thirty","fourty","fifty","sixty","seventy","eighty","ninety"]

convert1 :: Int -> String
convert1 n = units!!n

convert2 :: Int -> String
convert2 n
    | t==0 = units!!n
    | t==1 = teens!!n
    | u==0 = tens!!(t-2)
    | otherwise = tens!!(t-2) ++ "-" ++ units!!u
    where (t,u) = (n `div` 10,n `mod` 10)

convert3 :: Int -> String
convert3 n
    | h==0 = convert2 t
    | t==0 = units!!h ++ " hundred"
    | otherwise = units!!h ++ " hundred and " ++ convert2 t
    where (h,t) = (n `div` 100, n `mod` 100)

convert6 :: Int -> String
convert6 n
    | m==0 = convert3 h
    | h==0 = convert3 m ++ " thousand"
    | otherwise = convert3 m ++ " thousand" ++ link h ++ convert3 h
    where
        (m,h) = (n `div` 1000,n `mod` 1000)
        link :: Int -> String
        link h = if h < 100 then " and " else " "

convert :: Int -> String
convert = convert6



--anagrams :: Int -> [Word] -> String
