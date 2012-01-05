module SubSequences
    ( SubSequence(..)
    , maskSubSequences
    ) where

import Luhn
import Utils

data SubSequence = SubSequence
    { seqPrefix :: String
    , seqValue  :: String
    , seqSuffix :: String
    , seqIndex  :: Int
    , seqLength :: Int
    }

instance Show SubSequence where
    show (SubSequence pref val suf _ _) = pref ++ val ++ suf

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

subSequences :: String -> [SubSequence]
subSequences s = map (takeSubSequence s) $ getOffsets $ lengthDigits s

takeSubSequence :: String -> (Int, Int) -> SubSequence
takeSubSequence str (offset,limit) = let beg = takeDigits offset str
                                         rst = dropDigits offset str
                                         mid = takeDigits limit rst
                                         end = dropDigits limit rst
                                     in SubSequence beg mid end offset limit

getOffsets :: Int -> [(Int,Int)]
getOffsets = go 16

    where
        go :: Int -> Int -> [(Int,Int)]
        go n m
            | n >= 16 = zip [0..(m-16)] (repeat 16) ++ go 15 m
            | n >= 15 = zip [0..(m-15)] (repeat 15) ++ go 14 m
            | n >= 14 = zip [0..(m-14)] (repeat 14)
            | otherwise = [] -- invalid case

collapse :: [String] -> String
collapse []         = []
collapse [x]        = x
collapse (x1:x2:xs) = collapse $ (zipWith takeXs x1 x2) : xs

    where
        takeXs :: Char -> Char -> Char
        takeXs 'X' _  = 'X'
        takeXs  _ 'X' = 'X'
        takeXs  c  _  =  c
