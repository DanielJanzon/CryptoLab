module Random (random_nbit_int, random_int) where

import System.IO
import Data.Word
import qualified Data.ByteString.Lazy as BSL
import qualified NumberTheory as NT
import qualified Encoder as Enc

-- Return a random integer with num_bits bits
random_nbit_int :: Int -> IO Integer
random_nbit_int num_bits = do 
    contents <- BSL.readFile "/dev/urandom"
    let num = (Enc.words_to_int (BSL.unpack contents) ((div num_bits 8) + 1))
    return $ num `mod` (2^num_bits)

-- Return a random integer in the interval [0, n-1]
random_int :: Integer -> IO Integer
random_int n = do
    let num_bits = NT.get_num_bits n
    x <- random_nbit_int num_bits
    if x < n then return x else (random_int n)

