module Main where

import System.Environment
import qualified Data.ByteString.Lazy as BSL
import qualified NumberTheory as NT
import qualified Encoder as Enc

dispatch :: [(String, [String] ->IO())]
dispatch = [ ("pqprime", pqprime) ]

start = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

print_usage :: IOError -> IO()
print_usage err = do
    putStrLn "\tpqprime n\t- generate two random primes p, q such that p is n-bit and p=2q+1"

main = start `catch` print_usage

pqprime :: [String] -> IO()
pqprime [num_bits] = do
    contents <- BSL.readFile "/dev/urandom"
    let start = (Enc.words_to_int (BSL.unpack contents) (div (read num_bits) 8))
    let (p, q) = NT.next_pq_prime start
    putStrLn ((show p) ++ " " ++ (show q))

