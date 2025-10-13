module ChatSession
    ( SessionConfig(..)
    , runSession
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (IOException, catch, handle)
import Control.Monad (forever, unless, void, replicateM)
import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified CyclicGroup as CG
import qualified ElGamal as EG
import qualified Encoder
import qualified NumberTheory as NT
import qualified Random
import System.IO (BufferMode (LineBuffering), Handle, hClose, hFlush, hGetLine, hIsEOF, hPutStrLn, hSetBuffering, stdin)
import System.IO.Error (ioError, isEOFError)

-- | Configuration describing the static parameters of a chat session.
data SessionConfig = SessionConfig
    { sessionOrder     :: Integer
    , sessionGenerator :: Integer
    , sessionPrivate   :: Integer
    , sessionPadTarget :: Int
    }

-- | Drive a full duplex encrypted chat over an established handle.
runSession :: Handle -> SessionConfig -> IO ()
runSession handle cfg = do
    hSetBuffering handle LineBuffering
    let order = sessionOrder cfg
        generator = sessionGenerator cfg
        privateKey = sessionPrivate cfg
        padTarget = sessionPadTarget cfg
        group = CG.Group order
        generatorEl = CG.Element generator
        publicKey = NT.fast_pow generator privateKey order
    -- Perform the handshake exchanging group metadata and public keys.
    hPutStrLn handle (handshakeLine order generator publicKey)
    hFlush handle
    remoteHello <- waitHello handle
    let (remoteOrder, remoteGenerator, remotePublic) = remoteHello
    unless (remoteOrder == order && remoteGenerator == generator) $ do
        hPutStrLn handle "QUIT protocol-mismatch"
        hFlush handle
        error "Peer attempted to negotiate different group parameters"
    let remotePubEl = CG.Element remotePublic
    -- Channels that keep track of pad material.
    localPadChan <- newChan        -- Numbers received from peer, for encrypting outgoing messages.
    remotePadChan <- newChan       -- Numbers we generated for the peer, for decrypting inbound messages.
    remotePadCount <- newIORef 0   -- Number of pad integers currently outstanding with the peer.
    -- Spawn threads handling background responsibilities.
    _ <- forkIO $ outboundPadLoop handle group generatorEl remotePubEl order remotePadChan remotePadCount padTarget
    _ <- forkIO $ inboundLoop handle group privateKey order localPadChan remotePadChan remotePadCount
    userLoop handle order localPadChan
    hClose handle

handshakeLine :: Integer -> Integer -> Integer -> String
handshakeLine order generator publicKey = unwords ["HELLO", show order, show generator, show publicKey]

waitHello :: Handle -> IO (Integer, Integer, Integer)
waitHello handle = do
    line <- hGetLine handle
    case words line of
        ("HELLO":orderStr:genStr:pubStr:_) ->
            return (read orderStr, read genStr, read pubStr)
        _ -> waitHello handle

outboundPadLoop :: Handle -> CG.Group -> CG.Element -> CG.Element -> Integer -> Chan Integer -> IORef Int -> Int -> IO ()
outboundPadLoop handle group generatorEl remotePub order remotePadChan padCountRef padTarget =
    let loop = do
            current <- readIORef padCountRef
            if current >= padTarget
                then threadDelay 50000
                else do
                    pad <- Random.random_int order
                    tmp <- Random.random_int order
                    let (c1, c2) = EG.encrypt group (CG.Element pad) generatorEl tmp remotePub
                        CG.Element c1Val = c1
                        CG.Element c2Val = c2
                    hPutStrLn handle (unwords ["PAD", show c1Val, show c2Val])
                    hFlush handle
                    writeChan remotePadChan pad
                    atomicModifyIORef_' padCountRef (+1)
            loop
    in loop `catch` ignoreIO

inboundLoop :: Handle -> CG.Group -> Integer -> Integer -> Chan Integer -> Chan Integer -> IORef Int -> IO ()
inboundLoop handle group privateKey order localPadChan remotePadChan padCountRef =
    let process line =
            case words line of
                ["PAD", c1Str, c2Str] -> do
                    let cipher = (CG.Element (read c1Str), CG.Element (read c2Str))
                        CG.Element padVal = EG.decrypt group cipher privateKey
                    writeChan localPadChan padVal
                ("MSG":lenStr:blockCountStr:rest) -> do
                    let byteLen = read lenStr
                        blockCount = read blockCountStr
                        cipherBlocks = take blockCount (map read rest)
                    pads <- replicateM blockCount (readChan remotePadChan)
                    atomicModifyIORef_' padCountRef (\n -> max 0 (n - blockCount))
                    let plainBlocks = zipWith xor cipherBlocks pads
                        bytes = take byteLen $ Encoder.decode plainBlocks order
                        message = map (chr . fromIntegral) bytes
                    putStrLn ("< " ++ message)
                ("QUIT":_) -> do
                    putStrLn "Peer closed the connection."
                    ioError (userError "peer-quit")
                _ -> putStrLn ("Unrecognized frame: " ++ line)
    in handle (
            \e -> if isEOFError e then putStrLn "Peer disconnected." else return ()
        ) $ forever $ hGetLine handle >>= process

userLoop :: Handle -> Integer -> Chan Integer -> IO ()
userLoop handle order localPadChan = do
    eof <- hIsEOF stdin
    unless eof $ do
        line <- getLine
        if line == "/quit"
            then do
                hPutStrLn handle "QUIT"
                hFlush handle
            else do
                let bytes = map (fromIntegral . ord) line
                    blocks = Encoder.encode bytes order
                pads <- replicateM (length blocks) (readChan localPadChan)
                let cipherBlocks = zipWith xor blocks pads
                    frame = frameForMessage (length bytes) cipherBlocks
                hPutStrLn handle frame
                hFlush handle
                userLoop handle order localPadChan

frameForMessage :: Int -> [Integer] -> String
frameForMessage byteLen cipherBlocks =
    let header = ["MSG", show byteLen, show (length cipherBlocks)]
        payload = map show cipherBlocks
    in unwords (header ++ payload)

atomicModifyIORef_' :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_' ref f = void $ atomicModifyIORef' ref (\x -> (f x, ()))

ignoreIO :: IOException -> IO ()
ignoreIO _ = return ()
