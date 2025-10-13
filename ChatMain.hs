module Main where

import ChatSession (SessionConfig(..), runSession)
import Control.Exception (bracket)
import Network.Socket
    ( AddrInfo(..)
    , AddrInfoFlag(..)
    , Socket
    , SocketType(Stream)
    , addrAddress
    , addrFlags
    , addrSocketType
    , addrFamily
    , addrProtocol
    , bind
    , close
    , connect
    , defaultHints
    , getAddrInfo
    , listen
    , socket
    , withSocketsDo
    , accept
    )
import System.Environment (getArgs)
import System.IO (BufferMode(LineBuffering), IOMode(ReadWriteMode), hSetBuffering, socketToHandle)

usage :: IO ()
usage = do
    putStrLn "Usage:"
    putStrLn "  chat server <port> <order> <generator> <private-key>"
    putStrLn "  chat client <host> <port> <order> <generator> <private-key>"
    putStrLn "Type /quit to terminate the session."

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    case args of
        ["server", portStr, orderStr, generatorStr, privStr] ->
            runServer portStr (read orderStr) (read generatorStr) (read privStr)
        ["client", host, portStr, orderStr, generatorStr, privStr] ->
            runClient host portStr (read orderStr) (read generatorStr) (read privStr)
        _ -> usage

runServer :: String -> Integer -> Integer -> Integer -> IO ()
runServer port order generator priv = do
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
    bracket (openSocket addr) close $ \sock -> do
        bind sock (addrAddress addr)
        listen sock 1
        putStrLn $ "Listening on port " ++ port ++ "..."
        (conn, _) <- accept sock
        putStrLn "Client connected."
        handleConnection conn order generator priv
  where
    hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }

runClient :: String -> String -> Integer -> Integer -> Integer -> IO ()
runClient host port order generator priv = do
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    bracket (openSocket addr) close $ \sock -> do
        connect sock (addrAddress addr)
        putStrLn "Connected to server."
        handleConnection sock order generator priv
  where
    hints = defaultHints { addrSocketType = Stream }

handleConnection :: Socket -> Integer -> Integer -> Integer -> IO ()
handleConnection sock order generator priv = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering
    let cfg = SessionConfig
                { sessionOrder = order
                , sessionGenerator = generator
                , sessionPrivate = priv
                , sessionPadTarget = 256
                }
    runSession handle cfg

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
