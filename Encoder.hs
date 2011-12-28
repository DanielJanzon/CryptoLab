module Encoder
(words_to_int
,encode_number
,decode_number
,encode
,decode
) where

import Data.Word
import qualified Data.Char
import Debug.Trace

import NumberTheory

-- Craft an arbitrary sized integer from a byte string
encode_number = foldr (\x acc -> 256*acc + toInteger x) 0

-- Reverse the procedure to get a string from an integer
decode_number :: Integer -> [Word8]
decode_number 0 = []
decode_number n = (fromIntegral (mod n 256)) : decode_number (div n 256)

-- Create an integer out of num_bytes elements from the list 'words'
words_to_int :: [Word8] -> Int -> Integer
words_to_int words num_bytes = encode_number $ take num_bytes words

{-
  Encode a string of words into a list of Integers,
  where each list element is as large as possible,
  but always less than 'p'.
  
  The list of integers can be decoded into a list
  of bytes with the decode function.
-}

-- Helper function of the helper function
ench [] acc _ _ = (acc, [])
ench (x:xs) acc exp p = let val = (toInteger x)*exp+acc in
    if val < p then
        ench xs val (256*exp) p
    else
        (acc, x:xs)

-- Helper function
enc xs p = ench xs 0 1 p

encode :: [Word8] -> Integer -> [Integer]
encode [] _ = []
encode xs p = let (y, ys) = enc xs p in y : encode ys p

{-
  Reverse the procedure to get a string from a list
  of integers, no one larger than p.
-}

-- Helper function of the helper function
dech :: Integer -> Int -> [Word8]
dech n 0 = []
dech n k = let r = fromIntegral (mod n 256) in r : dech (div n 256) (k-1)

-- Helper function. Set k to the number of bytes fitting in n
dec n p = dech n (div (get_num_bits p) 8)

decode :: [Integer] -> Integer -> [Word8]
decode [] p = []
decode (x:xs) p = (dec x p) ++ decode xs p

