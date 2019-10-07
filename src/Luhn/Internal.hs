module Luhn.Internal
  ( toDigits
  , doubleEveryOther
  , sumDigits
  , fromDigits
  )
  where

toDigits :: Integer -> [Integer]
toDigits = go []
  where go digits n
          | n <= 0    = digits
          | otherwise = go (m:digits) d
            where (d,m) = divMod n 10

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse.single.reverse
  where
    single [] = []
    single (d:ds) = d:(double ds)
    double [] = []
    double (d:ds) = (2 * d):(single ds)

sumDigits :: [Integer] -> Integer
sumDigits = sum.flattenDigits
  where
    flattenDigits [] = []
    flattenDigits (n:ns) = (toDigits n) ++ (flattenDigits ns)

fromDigits :: [Integer] -> Integer
fromDigits = sum . zipWith (*) [10 ^ i | i <- [0..]] . reverse
