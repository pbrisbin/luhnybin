--
-- pbrisbin 2011
--
-- Tests pass but takes a little while, 88s or so.
--
module Main where

import Data.Char (isDigit, digitToInt)

data SubSequence = SubSequence
    { seqPrefix :: String
    , seqValue  :: String
    , seqSuffix :: String
    , seqIndex  :: Int
    , seqLength :: Int
    }

instance Show SubSequence where
    show (SubSequence pref val suf _ _) = pref ++ val ++ suf

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

-- |
--
-- The hard part.
--
-- Split the string up into all sequences of *digit* lengths 14 through
-- 16, mask each sub sequence, then collapse those masks down into one
-- overall mask.
--
-- In order to perform well, we must leverage the fact that masking any
-- N digit-length sequence means we don't need to look at any (< N)
-- digit-length sub sequences
--
maskSubSequences :: String -> String
maskSubSequences = collapse . map show . maskSubSequences' . subSequences

    where
        -- in order for the masking function to be optimized, the list
        -- of subsequence needs to come with inner subsequences
        -- immediately following their containing sequence before
        -- shifting.
        subSequences :: String -> [SubSequence]
        subSequences s = map (takeSubSequence s) $ getOffsets $ lengthDigits s

            where
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

        -- mask each subsequence we see, if we do mask something don't
        -- bother masking any following offsets of smaller lengths as
        -- they are assumed to be inner subsequences
        maskSubSequences' :: [SubSequence] -> [SubSequence]
        maskSubSequences' []     = []
        maskSubSequences' (s:ss) =
            let l     = seqLength s
                (b,v) = maskSubSequence s
            in v : maskSubSequences' (if b then dropWhile ((< l) . seqLength) ss else ss)

            where
                maskSubSequence :: SubSequence -> (Bool, SubSequence)
                maskSubSequence sq = let (b,v) = mask $ seqValue sq in (b, sq { seqValue = v })

                mask :: String -> (Bool, String)
                mask s = if luhnCheck (parseDigits s)
                            then (True, map hideDigit s) else (False, s)

                hideDigit :: Char -> Char
                hideDigit c
                    | isDigit c = 'X'
                    | otherwise = c

        -- take a list of masked strings and collapse them down so that
        -- any character masked in any of the strings is masked in the
        -- final output string
        collapse :: [String] -> String
        collapse []         = []
        collapse [x]        = x
        collapse (x1:x2:xs) = collapse $ (zipWith takeXs x1 x2) : xs

            where
                takeXs :: Char -> Char -> Char
                takeXs 'X' _  = 'X'
                takeXs  _ 'X' = 'X'
                takeXs  c  _  =  c

-- |
--
-- The Luhn check itself.
--
luhnCheck :: [Int] -> Bool
luhnCheck = (== 0) . (`mod` 10) . sum . reverse . doubleEveryOther . reverse

    where
        doubleEveryOther :: [Int] -> [Int]
        doubleEveryOther = concat . alternateMap (\x -> [x]) splitDouble

        alternateMap :: (a -> b) -> (a -> b) -> [a] -> [b]
        alternateMap f g = zipWith ($) (cycle [f, g])

        splitDouble :: Int -> [Int]
        splitDouble i = let product  = i*2
                            sproduct = show product
                        in if length sproduct == 2
                                -- note: need to reverse so the reversed
                                -- reversal works out correct :)
                                then reverse $ map digitToInt sproduct

                                -- note: assumes no products will be over 2
                                -- digits, valid since we're already limited to
                                -- 0..9 before calling this.
                                else [product]

-- |
-- 
-- Main program logic
--
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
