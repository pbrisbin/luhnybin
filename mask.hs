-- |
--
-- Full disclosure, I did see this[1]. Motivated by how short and simple
-- things could be, I tried to rewrite *without* going back for a closer
-- read.
--
-- Obviously there was influence, since things turned out very much the
-- same.
--
-- 1: https://github.com/szeiger/luhnybin/blob/master/luhny.hs
--
module Main where

import Data.Char (isDigit, digitToInt)
import Data.List (groupBy)

minLen   = 14
maxLen   = 16
maskChar = 'X'

maskStream :: String -> String
maskStream = concatMap (maskGroup 0) . groupBy (\a b -> isCcDigit a && isCcDigit b)

    where
        isCcDigit :: Char -> Bool
        isCcDigit = flip elem (['0'..'9'] ++ " -")

maskGroup :: Int -> String -> String
maskGroup _ [] = []
maskGroup i s@(c:cs) = let n = max i $ maxLuhnLen s in
    if isDigit c
        then when (n > 0) (const maskChar) c : maskGroup (n-1) cs
        else c : maskGroup i cs

    where
        when :: Bool -> (a -> a) -> a -> a
        when p f a = if p then f a else a

maxLuhnLen :: String -> Int
maxLuhnLen s = maximum $ map (\i -> tryLuhn i s) [minLen..maxLen]
    
    where
        tryLuhn :: Int -> String -> Int
        tryLuhn n s
            | length s < n = 0
            | otherwise    = if luhn n s then n else 0
            
        luhn :: Int -> String -> Bool
        luhn n s = luhnCheck . (map digitToInt) . take n $ filter isDigit s

luhnCheck :: [Int] -> Bool
luhnCheck = (== 0) . (`mod` 10) . sum
          . reverse . doubleEveryOther . reverse

    where
        doubleEveryOther :: [Int] -> [Int]
        doubleEveryOther = concat . alternateMap (\x -> [x]) (reverse . splitDouble)

        alternateMap :: (a -> b) -> (a -> b) -> [a] -> [b]
        alternateMap f g = zipWith ($) (cycle [f, g])

        splitDouble :: Int -> [Int]
        splitDouble = (\(a,b) -> [a,b]) . (`divMod` 10) . (*2)

main :: IO ()
main = interact maskStream 
