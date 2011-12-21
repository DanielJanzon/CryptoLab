module Encoder
(words_to_int
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

-- Create an integer out of num_bytes elements from the list 'words'
words_to_int :: [Word8] -> Int -> Integer
words_to_int words num_bytes = encode_number $ take num_bytes words
