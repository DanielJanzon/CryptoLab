module Main where

import qualified Encoder as Encoder
import Data.Word
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as BSL
import System.IO

main = do
    contents <- getContents
    BSL.putStr $ BSL.pack $ Encoder.decode (read contents) 63419
