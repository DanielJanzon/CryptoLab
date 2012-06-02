module Random (random_int) where

import System.IO
import Data.Word
import qualified Data.ByteString.Lazy as BSL
import qualified Encoder as Enc

random_int :: Int -> IO Integer
random_int num_bits = do 
    contents <- BSL.readFile "/dev/urandom"
    let num = (Enc.words_to_int (BSL.unpack contents) ((div num_bits 8) + 1))
    return $ num `mod` (2^num_bits)

