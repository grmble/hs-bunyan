{- | Bunyan Logging

A structured logging package like node's bunyan.
The output is compatible with bunyan.

This package defines the most basic API.

Also see bunyan-reader for using it in a RIO environment,
or bunyan-freer for freer-simple.
-}
module System.Log.Bunyan
  ( module X
  , rootLogger
  , namedLogger
  , withNamedLogger
  , withLogger
  , modifyContext
  , logRecord
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logTrace
  , logDuration
  , thenLogDuration
  , duration
  , consoleHandler
  , noopHandler
  , withLogWriter
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Monoid ((<>))
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Time.Clock.System
  ( SystemTime
  , getSystemTime
  , systemNanoseconds
  , systemSeconds
  , systemToUTCTime
  )
import Formatting ((%), int, sformat)
import Network.HostName (getHostName)
import System.IO
  ( BufferMode(..)
  , Handle
  , IOMode(..)
  , hPutStr
  , hPutStrLn
  , hSetBuffering
  , stderr
  , stdout
  , withFile
  )
import System.Log.Bunyan.LogText (LogText(..))
import System.Log.Bunyan.Types
import qualified System.Log.Bunyan.Types as X (Logger, Priority(..))
import System.Process.Current (getPid)
import UnliftIO.Async (async, cancel)
import UnliftIO.Exception (IOException, bracket, catchIO)
import UnliftIO.MVar (MVar, takeMVar)
import UnliftIO.STM (atomically, modifyTVar', newTVarIO, readTVarIO)

-- | Console handler function - prints to console
consoleHandler :: A.Object -> IO ()
consoleHandler obj
  -- putting a lazy bytestring does 2 writes, not good with multiple threads
  -- also do not flush explictly, rather rely on buffer mode
 = BSC8.hPut stdout (LBSC8.toStrict $ A.encode obj <> "\n")

-- | Handler that does not log anything
noopHandler :: A.Object -> IO ()
noopHandler _ = pure ()

-- | Run the action while running a log writing thread in the background
--
-- The log writer will write the entries from the mvar to the file.
-- A filepath of "-" is used for stderr.
--
-- If an error occurs writing to the file, it will try to use stderr
--
-- The handler function for the root logger is easy: '(putMVar xxx)'
withLogWriter :: MVar A.Object -> FilePath -> IO a -> IO a
withLogWriter q fp action = bracket (async $ logTo fp) cancel (const action)
  where
    handleError :: IOException -> IO ()
    handleError e = do
      hPutStrLn stderr ("Exception: " <> show e)
      go stderr
    logTo :: FilePath -> IO ()
    logTo "-" = go stderr
    logTo _ =
      do hPutStr
           stderr
           ("NOTE: log messages are being written to " <> fp <> "\n")
         withFile fp AppendMode $ \handle -> do
           hSetBuffering handle LineBuffering
           go handle
     `catchIO` handleError
    go :: Handle -> IO ()
    go handle = do
      x <- takeMVar q
      LBSC8.hPut handle (A.encode x <> "\n")
      go handle

-- | Create a root logger object.
-- |
-- | There should only be one root logger.
-- | It contains the handling function, all
-- | other loggers should be children of the root
-- | logger.
rootLogger :: MonadIO m => T.Text -> Priority -> (A.Object -> IO ()) -> m Logger
rootLogger n p h = do
  hn <- liftIO getHostName
  pid <- liftIO getPid
  priMap <- newTVarIO M.empty
  logSet <- newTVarIO S.empty
  return
    Logger
      { name = n
      , context = M.empty
      , handler = h
      , rootContext =
          M.fromList
            [ ("v", A.Number 0)
            , ("hostname", A.String (T.pack hn))
            , ("pid", A.Number $ fromIntegral pid)
            ]
      , priority = intPriority p
      , _priorityMap = priMap
      , _loggerNames = logSet
      }

-- | Create a named logger.
--
-- This determines the priority at creation time by
-- checking the shared log level config.
--
-- Note that a logger never changes its priority,
-- you have to create a new one for that.
namedLogger ::
     MonadIO m => T.Text -> (A.Object -> A.Object) -> Logger -> m Logger
namedLogger n ctxf lg = do
  m <- readTVarIO (_priorityMap lg)
  s <- readTVarIO (_loggerNames lg)
  unless (S.member n s) $
    atomically $ modifyTVar' (_loggerNames lg) (S.insert n)
  let pri = maybe (priority lg) intPriority $ M.lookup n m
  pure (lg {name = n, context = ctxf (context lg), priority = pri})

-- | Use the named logger in a block of code.
withNamedLogger ::
     MonadIO m
  => T.Text
  -> (A.Object -> A.Object)
  -> (Logger -> m a)
  -> Logger
  -> m a
withNamedLogger n ctxf action lg = namedLogger n ctxf lg >>= action

-- | Modify a loggers context.
--
-- The new logger will not check the logging config though,
-- it will keeps its parent's priority.
modifyContext :: (A.Object -> A.Object) -> Logger -> Logger
modifyContext f lg = lg {context = f (context lg)}

-- | Use the logger with changed context in a block of code
--
-- Like withNamedLogger, this will recheck the priority,
-- just without checking the name.
withLogger ::
     MonadIO m => (A.Object -> A.Object) -> (Logger -> m a) -> Logger -> m a
withLogger f action lg = withNamedLogger (name lg) f action lg

-- | Handle a finished log record via the root loggers handler
--
-- It has everything from it's loggers context
-- and root context, also the time will be filled in.
-- And of course, it has the message and priority
-- of it's creation.
handleRecord :: MonadIO m => A.Object -> Logger -> m ()
handleRecord obj lg = liftIO $ handler lg obj

-- | Log a message at level INFO - see logRecord for full API
logInfo :: MonadIO m => T.Text -> Logger -> m ()
logInfo = logRecord INFO id

-- | Log a message at level DEBUG - see logRecord for full API
logDebug :: MonadIO m => T.Text -> Logger -> m ()
logDebug = logRecord DEBUG id

-- | Log a message at level ERROR - see logRecord for full API
logError :: MonadIO m => T.Text -> Logger -> m ()
logError = logRecord ERROR id

-- | Log a message at level WARN - see logRecord for full API
logWarn :: MonadIO m => T.Text -> Logger -> m ()
logWarn = logRecord WARN id

-- | Log a message at level TRACE - see logRecord for full API
logTrace :: MonadIO m => T.Text -> Logger -> m ()
logTrace = logRecord TRACE id

-- | Log the duration of the action.
logDuration :: MonadIO m => (Logger -> m a) -> Logger -> m a
logDuration action = action `thenLogDuration` const pure

-- | Log the duration of the action
--
-- This will decorate the logger with the result of an additional function.
-- E.g. when logging a Warp request, you need the response that is produced
-- by the action to log a response status.
--
-- If an exception is thrown, nothing is logged.
thenLogDuration ::
     MonadIO m => (Logger -> m a) -> (a -> Logger -> m Logger) -> Logger -> m a
thenLogDuration action lgaction lg = do
  start <- liftIO getSystemTime
  a <- action lg
  end <- liftIO getSystemTime
  lg' <- lgaction a lg
  uncurry (logRecord INFO) (duration start end) lg'
  pure a

-- | Helper to compute a duration.
--
-- This will compute a suitable context object and message
-- which can be logged using
--
--    uncurry
--      (logRecord DEBUG) \
--      (duration <$> getSystemTime <*> getSystemTime)
duration :: SystemTime -> SystemTime -> (A.Object -> A.Object, T.Text)
duration start end = do
  let dur = double end - double start
  let ctx = M.insert "duration" (A.Number $ Scientific.fromFloatDigits dur)
  let msg = sformat ("completed in " % int % "ms") ((round $ 1000 * dur) :: Int)
  (ctx, msg)
  where
    double :: SystemTime -> Double
    double sc =
      fromIntegral (systemSeconds sc) +
      fromIntegral (systemNanoseconds sc) / 1e9

-- | Log a message at the given priority
--
-- It takes a function to modify the context (hint: M.insert) and a message.
logRecord ::
     MonadIO m => Priority -> (A.Object -> A.Object) -> T.Text -> Logger -> m ()
logRecord pri f msg lg = do
  let pri' = intPriority pri
  when (pri' >= priority lg) $ do
    tm <- liftIO getSystemTime
    handleRecord (decorateRecord pri f msg tm lg) lg

-- | Helper function that produces the logged json record
decorateRecord ::
     Priority
  -> (A.Object -> A.Object)
  -> T.Text
  -> SystemTime
  -> Logger
  -> A.Object
decorateRecord pri f msg tm lg =
  M.insert "name" (A.String $ name lg) $
  M.insert "level" (A.Number $ fromIntegral $ intPriority pri) $
  M.insert "msg" (A.String (toText msg)) $
  M.insert "time" (A.toJSON $ systemToUTCTime tm) $
  M.union (rootContext lg) (f $ context lg)
