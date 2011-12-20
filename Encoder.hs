module Encoder
(byte_str_to_odd
,encode_number
,decode_number
) where

import Data.Word
--import Data.Char

-- Craft an arbitrary sized integer from a byte string
encode_number = foldr (\x acc -> 256*acc + toInteger x) 0

-- Reverse the procedure to get a string from an integer
decode_number :: Integer -> [Word8]
decode_number 0 = []
decode_number n = (fromIntegral (mod n 256)) : decode_number (div n 256)

oddify x = if x `mod` 2 == 1 then x else x+1

byte_str_to_odd :: [Word8] -> Int -> Integer
byte_str_to_odd str num_bytes = oddify (encode_number (take num_bytes str))
