module Luhn (luhnCheck) where

import Utils

luhnCheck :: [Int] -> Bool
luhnCheck = (== 0) . (`mod` 10) . sum
          . reverse . doubleEveryOther . reverse

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther = concat . alternateMap (\x -> [x]) splitDouble

alternateMap :: (a -> b) -> (a -> b) -> [a] -> [b]
alternateMap f g = zipWith ($) (cycle [f, g])

splitDouble :: Int -> [Int]
splitDouble i =
    let product  = i*2
        sproduct = show product
    in if length sproduct == 2
            -- note: need to reverse so the reversed reversal works out
            -- correct :)
            then reverse $ map digitToInt sproduct

            -- note: assumes no products will be over 2 digits, valid
            -- since we're already limited to 0..9 before calling this.
            else [product]
