module Main where

import qualified Encoder as Encoder
import qualified Data.ByteString.Lazy as BSL
import System.IO

main = do
    contents <- BSL.getContents
    putStrLn $ show $ Encoder.encode (BSL.unpack contents) 63419