-- TODO:
--
--   hardcoded at 14 min, 16 max; parameterize these
--
module SubSequences
    ( SubSequence(..)
    , subSequences
    ) where

import Utils
import Data.List (sortBy, nub)

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
subSequences s = map (takeSubSequence s) $ sortOffsets $ nub $ getOffsets $ lengthDigits s

    where
        takeSubSequence :: String -> (Int, Int) -> SubSequence
        takeSubSequence str (offset,limit) = let beg = takeDigits offset str
                                                 rst = dropDigits offset str
                                                 mid = takeDigits limit rst
                                                 end = dropDigits limit rst
                                             in SubSequence beg mid end offset limit

-- | Sorts index ascending, length decending. This way inner sequences
--   follow their containers.
sortOffsets :: [(Int,Int)] -> [(Int,Int)]
sortOffsets = sortBy cmp

    where
        cmp (a1,b1) (a2,b2) =
            case compare a1 a2 of
                EQ -> compare b2 b1
                x  -> x

getOffsets :: Int -> [(Int,Int)]
getOffsets = go 16

    where
        go :: Int -> Int -> [(Int,Int)]
        go n m
            | n >= 16 = zip [0..(m-16)] (repeat 16) ++ go 15 m
            | n >= 15 = zip [0..(m-15)] (repeat 15) ++ go 14 m
            | n >= 14 = zip [0..(m-14)] (repeat 14)
            | otherwise = [] -- invalid case
