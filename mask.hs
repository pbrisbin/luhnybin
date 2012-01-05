-- |
--
-- pbrisbin 2011
--
module Main where

import Utils
import SubSequences
import Luhn

main :: IO ()
main = interact (maskStream [])

maskStream :: String -> String -> String
maskStream acc []     = maskIf ((>= 14) . lengthDigits) acc
maskStream acc (x:xs) = if isCcDigit x
                            then maskStream (acc ++ [x]) xs
                            else (maskIf ((>= 14) . lengthDigits) acc) ++ [x] ++ (maskStream [] xs)

isCcDigit :: Char -> Bool
isCcDigit = flip elem (['0'..'9'] ++ " -")

maskIf :: (String -> Bool) -> String -> String
maskIf p s = if p s then maskSubSequences s else s

maskSubSequences :: String -> String
maskSubSequences = collapse . map show . maskSubSequences' . subSequences

    where
        maskSubSequences' :: [SubSequence] -> [SubSequence]
        maskSubSequences' []     = []
        maskSubSequences' (s:ss) =
            let l     = seqLength s
                (b,v) = maskSubSequence s
            in v : maskSubSequences' (if b then dropWhile ((< l) . seqLength) ss else ss)

maskSubSequence :: SubSequence -> (Bool, SubSequence)
maskSubSequence sq = let (b,v) = mask $ seqValue sq in (b, sq { seqValue = v })

mask :: String -> (Bool, String)
mask s = if luhnCheck (parseDigits s)
            then (True, map hideDigit s) else (False, s)

hideDigit :: Char -> Char
hideDigit c
    | isDigit c = 'X'
    | otherwise = c

collapse :: [String] -> String
collapse []         = []
collapse [x]        = x
collapse (x1:x2:xs) = collapse $ (zipWith takeXs x1 x2) : xs

    where
        takeXs :: Char -> Char -> Char
        takeXs 'X' _  = 'X'
        takeXs  _ 'X' = 'X'
        takeXs  c  _  =  c
