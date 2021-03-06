module Main where

import System.Environment
import Control.Concurrent
import Control.Exception
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified NumberTheory as NT
import qualified Encoder as Enc
import qualified Random as Rnd

dispatch :: [(String, [String] ->IO())]
dispatch = [ ("pqprime", pqprime),
             ("prime", prime),
             ("pprime", pprime),
             ("nbitint", nbitint),
             ("int", genint) ]

start = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

print_usage :: IOError -> IO()
print_usage err = do
    putStrLn "\tnbitint n \t- generate a random n-bit integer"
    putStrLn "\tint n     \t- generate a random integer less than n"
    putStrLn "\tprime n   \t- generate a random n-bit prime"
    putStrLn "\tpprime n m\t- generate a random n-bit prime with m parallel search threads,"
    putStrLn "\t              must be run with +RTS -N<m>"
    putStrLn "\tpqprime n \t- generate two random primes p, q such that p is n-bit and p=2q+1"

main = start `catch` print_usage

display_maybe_result :: Maybe Integer -> IO()
display_maybe_result result = do
    if result == Nothing then
        putStrLn "search failed, try again"
    else
        putStrLn (show (Maybe.fromJust result))

pqprime :: [String] -> IO()
pqprime [num_bits] = do
    start <- Rnd.random_int (read num_bits)
    let (p, q) = NT.next_pq_prime start
    putStrLn ((show p) ++ " " ++ (show q))

prime :: [String] -> IO()
prime [num_bits] = do
    start <- Rnd.random_int $ 2^(read num_bits)
    let p = NT.next_nbit_prime start (read num_bits)
    display_maybe_result p

nbitint :: [String] -> IO()
nbitint [num_bits] = do
    x <- Rnd.random_nbit_int (read num_bits)
    putStrLn $ show x

genint :: [String] -> IO()
genint [n] = do
    x <- Rnd.random_int (read n)
    putStrLn $ show x


{-
   A parallel version of the prime search and its auxillary
   functions. The idea is to let loose several threads that
   all write to the same MVar if/when they are finished.
   The main thread blocks on reading the MVar and when there
   is anything to read it cancels the other threads (and
   the finished thread too which is no harm done).
-}
cancel threadId = throwTo threadId ThreadKilled

race_for_prime m bits startValue = forkIO $ do
    let r = NT.next_nbit_prime startValue bits
    putMVar m $! r -- Forcing evaluation with $! is crucial

pprime :: [String] -> IO()
pprime [num_bits_str, num_threads_str] = do
    let num_bits = read num_bits_str
    let num_threads = read num_threads_str
    start <- mapM Rnd.random_int $ take num_threads $ repeat (2^num_bits)
    m <- newEmptyMVar

    threadIds <- mapM (race_for_prime m num_bits) start 

    winner <- takeMVar m
    mapM cancel threadIds

    display_maybe_result winner
