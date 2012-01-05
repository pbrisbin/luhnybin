module Utils
    ( lengthDigits
    , takeDigits
    , dropDigits
    , parseDigits
    , isCcDigit
    , when
    , module Data.Char
    ) where

import Data.Char (isDigit, digitToInt)

-- |
--
-- > lengthDigits "-1-2 3 4-5-" => 5
--
lengthDigits :: String -> Int
lengthDigits = length . filter isDigit

-- |
--
-- > takeDigits 2 "-1-2 3 4-5-" => "-1-2"
--
takeDigits :: Int -> String -> String
takeDigits n = go n []

    where
        go :: Int -> String -> String -> String
        go _ acc [] = acc
        go i acc (x:xs)
            | lengthDigits acc >= i = acc
            | otherwise = go i (acc ++ [x]) xs

-- |
--
-- > dropDigits 2 "-1-2 3 4-5-" => " 3 4-5-"
--
dropDigits :: Int -> String -> String
dropDigits n = go n 0

    where
        go :: Int -> Int -> String -> String
        go i dropped [] = []
        go i dropped s@(x:xs)
            | dropped >= i = s
            | otherwise = if isDigit x
                            then go i (dropped+1) xs
                            else go i dropped xs

-- |
--
-- > parseDigits "-1-2 3 4-5-" => [1,2,3,4,5]
--
parseDigits :: String -> [Int]
parseDigits = map digitToInt . filter isDigit

-- | As per the exercise, CC strings are digits, spaces, and/or hyphens
isCcDigit :: Char -> Bool
isCcDigit = flip elem (['0'..'9'] ++ " -")

-- | Apply a function only when a predicate holds
when :: (a -> Bool) -> (a -> a) -> a -> a
when p f a = if p a then f a else a
