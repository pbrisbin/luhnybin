-- |
--
-- A SubSequence is use to manipulate a substring of characters while
-- retaining information about what precedes and follows in the overall
-- string so it can be reconstructed post-manipulation.
--
-- This module is responsible for taking a string of characters and
-- providing all the subsequences of interesting lengths (where the
-- lengths are determined by number of *digit* characters).
--
-- The list of SubSequences is returned in such a way that inner
-- subsequences are listed immediately after their containers. This
-- allows operations (such as masking) to be optimized by smartly
-- skipping inner sequences when appropriate.
--
-- TODO:
--
--   getOffsets should return things in the correct order
--
--   subSequences, etc are hardcoded at 14 min, 16 max; parameterize
--   these
--
module SubSequences
    ( SubSequence(..)
    , subSequences
    ) where

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
