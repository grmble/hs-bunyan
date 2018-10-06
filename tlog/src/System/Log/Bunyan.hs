{-# LANGUAGE TemplateHaskell #-}

{- | Bunyan Logging

A structured logging package like node's bunyan.
The output is compatible with bunyan.

This is the implementation as Reader in IO.
-}
module System.Log.Bunyan
  ( module Data.Time.Clock.System
  , HasLogger(..)
  , Priority(..)
  , Logger
  , LogRecord(..)
  , rootLogger
  , childLogger
  , localLogger
  , logRecord
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logTrace
  , logDuration
  , duration
  , consoleHandler
  , noopHandler
  , loggerNames'
  , priorityMap'
  ) where

import Control.Lens.PicoLens (Lens', over, view)
import Control.Monad (unless, when)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as ATH
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Time.Clock.System (SystemTime, getSystemTime)
import qualified Data.Time.Clock.System as C
import GHC.Generics
import Network.BSD (getHostName)
import System.IO (hFlush, stdout)
import System.Process.Current (getPid)
import Text.Printf (printf)
import Text.Show.Functions ()
import UnliftIO.STM

-- | Symbolic priorities
data Priority
  = FATAL
  | ERROR
  | WARN
  | INFO
  | DEBUG
  | TRACE
  deriving (Generic, Show, Eq)

$(ATH.deriveJSON A.defaultOptions ''Priority)

-- | Int values for the possible priorities
--
-- These are the same as the bunyan values
intPriority :: Priority -> Int
intPriority FATAL = 60
intPriority ERROR = 50
intPriority WARN = 40
intPriority INFO = 30
intPriority DEBUG = 20
intPriority TRACE = 10

-- | A Logger has a name, priority and context.
--
-- The context is a default JSON object -
-- everything in here will be in every log message
-- unless overwritten by the log record's context.
--
-- Everything else is shared between all loggers.
data Logger = Logger
  { name :: {-# UNPACK #-}!T.Text
    -- ^ logger name
  , context :: !AT.Object
    -- ^ current context object - can be overwritten
  , priority :: {-# UNPACK #-}!Int
    -- ^ log level of the logger
  , handler :: !(LogRecord -> IO ())
    -- ^ handler for log records
  , rootContext :: !AT.Object
    -- ^ root context - coKntains everything that can be determined
    -- when the root logger is created.  Cant be overwritten.
  , priorityMap :: {-# UNPACK #-}!(TVar (M.HashMap T.Text Priority))
    -- ^ map of logger names to priority
  , loggerNames :: {-# UNPACK #-}!(TVar (S.HashSet T.Text))
  }

instance Show Logger where
  show lg =
    "Logger name=" ++
    show (name lg) ++
    "\n    context=" ++
    show (context lg) ++
    "\n    rootContext=" ++
    show (rootContext lg) ++ "\n    priority=" ++ show (priority lg)

-- | Lens typeclass for getting a logger
class HasLogger a where
  logger :: Lens' a Logger

instance HasLogger Logger where
  logger = id

loggerNames' :: Lens' Logger (TVar (S.HashSet T.Text))
loggerNames' k lg = fmap (\x -> lg {loggerNames = x}) (k (loggerNames lg))

priorityMap' :: Lens' Logger (TVar (M.HashMap T.Text Priority))
priorityMap' k lg = fmap (\x -> lg {priorityMap = x}) (k (priorityMap lg))

-- | Console handler function - prints to console
consoleHandler :: LogRecord -> IO ()
consoleHandler (LogRecord obj) = do
  LBSC8.putStrLn $ A.encode obj
  hFlush stdout

-- | Handler that does not log anything
noopHandler :: LogRecord -> IO ()
noopHandler _ = pure ()

-- | Create a root logger object.
-- |
-- | There should only be one root logger.
-- | It contains the handling function, all
-- | other loggers should be children of the root
-- | logger.
rootLogger ::
     MonadIO m => T.Text -> Priority -> (LogRecord -> IO ()) -> m Logger
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
      , priorityMap = priMap
      , loggerNames = logSet
      }

-- | Create a child logger with the given name and default properties
--
-- At creation time, it will read shared config for the
-- loglevel of the given name.  The decision to log or not
-- is simply an integer comparison - so try to have long lived
-- child loggers.
childLogger ::
     (HasLogger r, MonadReader r m, MonadIO m)
  => T.Text
  -> AT.Object
  -> m Logger
childLogger n ctx = do
  lg <- asks (view logger)
  m <- readTVarIO (priorityMap lg)
  s <- readTVarIO (loggerNames lg)
  unless (S.member n s) $ atomically $ modifyTVar (loggerNames lg) (S.insert n)
  let pri = maybe (priority lg) intPriority $ M.lookup n m
  pure lg {name = n, context = M.union ctx (context lg), priority = pri}

-- | A log record.
--
-- It has everything from it's loggers context
-- and root context, also the time will be filled in.
-- And of course, it has the message and priority
-- of it's creation.
newtype LogRecord =
  LogRecord AT.Object
  deriving (Show, Eq)

-- | Log a message at level INFO - see logRecord for full API
logInfo :: (HasLogger r, MonadReader r m, MonadIO m) => T.Text -> m ()
logInfo = logRecord INFO M.empty

-- | Log a message at level DEBUG - see logRecord for full API
logDebug :: (HasLogger r, MonadReader r m, MonadIO m) => T.Text -> m ()
logDebug = logRecord DEBUG M.empty

-- | Log a message at level ERROR - see logRecord for full API
logError :: (HasLogger r, MonadReader r m, MonadIO m) => T.Text -> m ()
logError = logRecord ERROR M.empty

-- | Log a message at level WARN - see logRecord for full API
logWarn :: (HasLogger r, MonadReader r m, MonadIO m) => T.Text -> m ()
logWarn = logRecord WARN M.empty

-- | Log a message at level TRACE - see logRecord for full API
logTrace :: (HasLogger r, MonadReader r m, MonadIO m) => T.Text -> m ()
logTrace = logRecord TRACE M.empty

-- | Log the duration of the action.
logDuration :: (HasLogger r, MonadReader r m, MonadIO m) => m a -> m a
logDuration action = do
  start <- liftIO C.getSystemTime
  a <- action
  end <- liftIO C.getSystemTime
  uncurry (logRecord INFO) (duration start end)
  pure a

-- | Helper to compute a duration.
--
-- This will compute a suitable context object and message
-- which can be logged using
--
--    uncurry
--      (logRecord DEBUG) \
--      (duration <$> getSystemTime <*> getSystemTime)
duration :: SystemTime -> SystemTime -> (AT.Object, T.Text)
duration start end = do
  let dur = double end - double start
  let ctx = M.singleton "duration" (AT.Number $ Scientific.fromFloatDigits dur)
  let msg = T.pack $ printf "completed in %dms" ((round $ 1000 * dur) :: Int)
  (ctx, msg)
  where
    double :: SystemTime -> Double
    double sc =
      fromIntegral (C.systemSeconds sc) +
      fromIntegral (C.systemNanoseconds sc) / 1e9

-- | Call the IO actoin with a local childlogger
localLogger ::
     (HasLogger r, MonadReader r m, MonadIO m)
  => T.Text
  -> AT.Object
  -> m a
  -> m a
localLogger n ctx action = do
  lg <- childLogger n ctx
  local (over logger (const lg)) action

-- | Log a json record to the rootLoggers handler
logRecord ::
     (HasLogger r, MonadReader r m, MonadIO m)
  => Priority
  -> AT.Object
  -> T.Text
  -> m ()
logRecord pri obj msg = do
  lg <- asks (view logger)
  let pri' = intPriority pri
  when (pri' >= priority lg) $ do
    tm <- liftIO C.getSystemTime
    liftIO $ handler lg (LogRecord (decorate lg tm (context lg)))
  where
    decorate lg t =
      M.insert "name" (A.String $ name lg) .
      M.insert "level" (A.Number $ fromIntegral $ intPriority pri) .
      M.insert "msg" (A.String msg) .
      M.union obj .
      M.insert "time" (A.toJSON $ C.systemToUTCTime t) .
      M.union (rootContext lg)
