module Encoder
(
byte_str_to_odd
) where

import Data.Word

-- Craft an odd arbitrary sized integer from a byte string
make_number [] = 0
make_number (x:xs) = x + (make_number (map (256*) xs))

byte_to_num str 0 = []
byte_to_num (x:xs) k = toInteger x : byte_to_num xs (k-1)

oddify x = if x `mod` 2 == 1 then x else x+1

byte_str_to_odd :: [Word8] -> Integer -> Integer
byte_str_to_odd str num_bytes = oddify (make_number (byte_to_num str num_bytes))
