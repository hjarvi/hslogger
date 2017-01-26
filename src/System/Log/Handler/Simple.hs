{- |
   Module     : System.Log.Handler.Simple
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Portability: portable

Simple log handlers

Written by John Goerzen, jgoerzen\@complete.org
-}

module System.Log.Handler.Simple(streamHandler, fileHandler,
                                      GenericHandler (..))
    where

import Control.Exception (tryJust)
import Control.DeepSeq
import Data.Char (ord)

import System.Log
import System.Log.Handler
import System.Log.Formatter
import System.IO (Handle, openFile, hClose, hFlush, IOMode(..))
import System.IO.Error
import Control.Concurrent.MVar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Char8 (hPutStrLn)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LB

toByteString = LB.toStrict . toLazyByteString
showB = byteString . BSC.pack . show

{- | A helper data type. -}

data GenericHandler a = GenericHandler {priority :: Priority,
                                        formatter :: LogFormatter (GenericHandler a),
                                        privData :: a,
                                        writeFunc :: a -> LogString -> IO (),
                                        closeFunc :: a -> IO () }

instance LogHandler (GenericHandler a) where
    setLevel sh p = sh{priority = p}
    getLevel sh = priority sh
    setFormatter sh f = sh{formatter = f}
    getFormatter sh = formatter sh
    emit sh (_,msg) _ = (writeFunc sh) (privData sh) msg
    close sh = (closeFunc sh) (privData sh)


{- | Create a stream log handler.  Log messages sent to this handler will
   be sent to the stream used initially.  Note that the 'close' method
   will have no effect on stream handlers; it does not actually close
   the underlying stream.  -}

streamHandler :: Handle -> Priority -> IO (GenericHandler Handle)
streamHandler h pri =
    do lock <- newMVar ()
       let mywritefunc hdl msg =
               msg `deepseq`
               withMVar lock (\_ -> do writeToHandle hdl msg
                                       hFlush hdl
                             )
       return (GenericHandler {priority = pri,
                               formatter = nullFormatter,
                               privData = h,
                               writeFunc = mywritefunc,
                               closeFunc = \_ -> return ()})
    where
      writeToHandle hdl msg = do
          rv <- tryJust myException (hPutStrLn hdl msg)
          either (handleWriteException hdl msg) return rv
      myException e
          | isDoesNotExistError e = Just e
          | otherwise = Nothing
      handleWriteException hdl msg e =
          let msg' = toByteString
                     $ mconcat [ byteString $ BSC.pack "Error writing log message: "
                               , showB e
                               , byteString $ BSC.pack " (original message: "
                               , byteString msg
                               , byteString $ BSC.pack ")"
                               ]
          in hPutStrLn hdl msg'

{- | Create a file log handler.  Log messages sent to this handler
   will be sent to the filename specified, which will be opened
   in Append mode.  Calling 'close' on the handler will close the file.
   -}

fileHandler :: FilePath -> Priority -> IO (GenericHandler Handle)
fileHandler fp pri = do
                     h <- openFile fp AppendMode
                     sh <- streamHandler h pri
                     return (sh{closeFunc = hClose})

