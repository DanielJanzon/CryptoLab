module Main where

import System.Environment
import Control.Exception
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified NumberTheory as NT
import qualified Encoder as Enc
import qualified Random as Rnd

dispatch :: [(String, [String] ->IO())]
dispatch = [ ("pqprime", pqprime),
             ("prime", prime),
             ("nbitint", nbitint),
             ("int", genint) ]

start = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

print_usage :: IOError -> IO()
print_usage err = do
    putStrLn "\tnbitint n\t- generate a random n-bit integer"
    putStrLn "\tint n    \t- generate a random integer less than n"
    putStrLn "\tprime n  \t- generate a random n-bit prime"
    putStrLn "\tpqprime n\t- generate two random primes p, q such that p is n-bit and p=2q+1"

main = start `catch` print_usage

pqprime :: [String] -> IO()
pqprime [num_bits] = do
    start <- Rnd.random_int (read num_bits)
    let (p, q) = NT.next_pq_prime start
    putStrLn ((show p) ++ " " ++ (show q))

prime :: [String] -> IO()
prime [num_bits] = do
    start <- Rnd.random_int $ 2^(read num_bits)
    let p = NT.next_nbit_prime start (read num_bits)
    if p == Nothing then
        putStrLn "search failed, try again"
    else
        putStrLn (show (Maybe.fromJust p))

nbitint :: [String] -> IO()
nbitint [num_bits] = do
    x <- Rnd.random_nbit_int (read num_bits)
    putStrLn $ show x

genint :: [String] -> IO()
genint [n] = do
    x <- Rnd.random_int (read n)
    putStrLn $ show x


