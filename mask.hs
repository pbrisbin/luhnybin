-- |
--
-- pbrisbin 2011
--
-- Tests pass but takes a little while, 88s or so.
--
module Main where

import Utils
import SubSequences

main :: IO ()
main = interact (maskStream [])

    where
        maskStream :: String -> String -> String
        maskStream acc []     = maskIf ((>= 14) . lengthDigits) acc
        maskStream acc (x:xs) = if isCcDigit x
                                    then maskStream (acc ++ [x]) xs
                                    else (maskIf ((>= 14) . lengthDigits) acc) ++ [x] ++ (maskStream [] xs)

        isCcDigit :: Char -> Bool
        isCcDigit = flip elem (['0'..'9'] ++ " -")

        maskIf :: (String -> Bool) -> String -> String
        maskIf p s = if p s then maskSubSequences s else s
