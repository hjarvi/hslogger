{-
Copyright (c) 2005-2011 John Goerzen
License: BSD3
-}
{- |

Definition of log formatter support

A few basic, and extendable formatters are defined.


Please see "System.Log.Logger" for extensive documentation on the
logging system.

-}
{-# LANGUAGE ViewPatterns #-}

module System.Log.Formatter( LogFormatter
                           , nullFormatter
                           , simpleLogFormatter
                           , tfLogFormatter
                           , varFormatter
                           ) where
import Prelude hiding (uncons, isPrefixOf, drop, length)
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
import Data.List hiding (uncons, length, isPrefixOf, drop)
import Control.Applicative ((<$>))
import Control.Concurrent (myThreadId)
#ifndef mingw32_HOST_OS
import System.Posix.Process (getProcessID)
#endif

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import Data.Time (getZonedTime,getCurrentTime,formatTime)

import System.Log

-- | A LogFormatter is used to format log messages.  Note that it is paramterized on the
-- 'Handler' to allow the formatter to use information specific to the handler
-- (an example of can be seen in the formatter used in 'System.Log.Handler.Syslog')
type LogFormatter a = a -- ^ The LogHandler that the passed message came from 
                    -> LogRecord -- ^ The log message and priority
                    -> String -- ^ The logger name
                    -> IO LogString -- ^ The formatted log message

showB :: (Show a) => a -> BS.ByteString
showB = BSC.pack . show

-- | Returns the passed message as is, ie. no formatting is done.
nullFormatter :: LogFormatter a
nullFormatter _ (_,msg) _ = return msg

-- | Takes a format string, and returns a formatter that may be used to
--   format log messages.  The format string may contain variables prefixed with
--   a $-sign which will be replaced at runtime with corresponding values.  The 
--   currently supported variables are:
--
--    * @$msg@ - The actual log message
--
--    * @$loggername@ - The name of the logger
--
--    * @$prio@ - The priority level of the message
--
--    * @$tid@  - The thread ID
--
--    * @$pid@  - Process ID  (Not available on windows)
--
--    * @$time@ - The current time 
--
--    * @$utcTime@ - The current time in UTC Time
simpleLogFormatter :: LogString -> LogFormatter a
simpleLogFormatter format h (prio, msg) loggername = 
    tfLogFormatter "%F %X %Z" format h (prio,msg) loggername

-- | Like 'simpleLogFormatter' but allow the time format to be specified in the first
-- parameter (this is passed to 'Date.Time.Format.formatTime')
tfLogFormatter :: String -> LogString -> LogFormatter a
tfLogFormatter timeFormat format = do
  varFormatter [(BSC.pack "time", BSC.pack <$> (formatTime defaultTimeLocale timeFormat <$> getZonedTime))
               ,(BSC.pack "utcTime", BSC.pack <$> (formatTime defaultTimeLocale timeFormat <$> getCurrentTime))
               ]
      format

-- | An extensible formatter that allows new substition /variables/ to be defined.
-- Each variable has an associated IO action that is used to produce the
-- string to substitute for the variable name.  The predefined variables are the same
-- as for 'simpleLogFormatter' /excluding/ @$time@ and @$utcTime@.
varFormatter :: [(LogString, IO LogString)] -> LogString -> LogFormatter a
varFormatter vars format h (prio,msg) loggername = do
    outmsg <- replaceVarM (vars++[(BSC.pack "msg", return msg)
                                 ,(BSC.pack "prio", return $ showB prio)
                                 ,(BSC.pack "loggername", return $ BSC.pack loggername)
                                 ,(BSC.pack "tid", showB <$> myThreadId)
#ifndef mingw32_HOST_OS
                                 ,(BSC.pack "pid", showB <$> getProcessID)
#endif
                                 ]
                          ) 
                  format
    return outmsg


-- | Replace some '$' variables in a string with supplied values
replaceVarM :: [(LogString, IO LogString)] -- ^ A list of (variableName, action to get the replacement string) pairs
           -> LogString   -- ^ String to perform substitution on
           -> IO LogString   -- ^ Resulting string
replaceVarM _ (BSC.uncons -> Nothing) = return empty
replaceVarM keyVals (BSC.uncons -> Just (s, ss)) | s=='$' =
      do (f,rest) <- replaceStart keyVals ss
         repRest <- replaceVarM keyVals rest
         return $ f `append` repRest
                                       | otherwise = replaceVarM keyVals ss >>= return . (append (BSC.pack [s]))
    where
      replaceStart :: [(LogString, IO LogString)] -> LogString -> IO (LogString,LogString)
      replaceStart [] str = return (BSC.pack "$",str)
      replaceStart ((k,v):kvs) str | k `isPrefixOf` str = do vs <- v
                                                             return (vs, drop (length k) str)
                                   | otherwise = replaceStart kvs str
                

