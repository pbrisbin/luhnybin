module Main where

import Utils
import SubSequences
import Luhn

maskStream :: String -> String -> String
maskStream acc []     = when ((>= 14) . lengthDigits) maskSubSequences acc
maskStream acc (x:xs) = if isCcDigit x
                            then maskStream (acc ++ [x]) xs
                            else (when ((>= 14) . lengthDigits) maskSubSequences acc) ++ [x] ++ (maskStream [] xs)

maskSubSequences :: String -> String
maskSubSequences = collapse . map show . maskSubSequences' . subSequences

    where
        maskSubSequences' :: [SubSequence] -> [SubSequence]
        maskSubSequences' []     = []
        maskSubSequences' (s:ss) =
            let l     = seqLength s
                (b,v) = maskSubSequence s
            in v : maskSubSequences' (if b then dropWhile ((< l) . seqLength) ss else ss)

        collapse :: [String] -> String
        collapse []         = []
        collapse [x]        = x
        collapse (x1:x2:xs) = collapse $ (zipWith takeXs x1 x2) : xs

            where
                takeXs :: Char -> Char -> Char
                takeXs 'X' _  = 'X'
                takeXs  _ 'X' = 'X'
                takeXs  c  _  =  c

maskSubSequence :: SubSequence -> (Bool, SubSequence)
maskSubSequence sq = let (b,v) = mask $ seqValue sq in (b, sq { seqValue = v })

    where
        mask :: String -> (Bool, String)
        mask s = if luhnCheck (parseDigits s)
                    then (True, map (when isDigit hide) s) else (False, s)

        hide :: Char -> Char
        hide _ = 'X'

main :: IO ()
main = interact (maskStream [])
