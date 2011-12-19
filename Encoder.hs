module Encoder
(
byte_str_to_odd
) where

import Data.Word

-- Craft an odd arbitrary sized integer from a byte string
make_number = foldl (\acc x -> 256*acc + toInteger x) 0

oddify x = if x `mod` 2 == 1 then x else x+1

byte_str_to_odd :: [Word8] -> Int -> Integer
byte_str_to_odd str num_bytes = oddify (make_number (take num_bytes str))
