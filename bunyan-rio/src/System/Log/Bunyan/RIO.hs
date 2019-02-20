{- | Bunyan RIO Implementation
-}
module System.Log.Bunyan.RIO
  ( module System.Log.Bunyan
  , module Data.Time.Clock.System
  , HasLogger(..)
  , MonadBunyan(..)
  , localLogger
  , logInfo
  , logDebug
  , logError
  , logWarn
  , logTrace
  , logDuration
  , logRecord
  ) where

import Control.Lens.PicoLens (Lens', over, view)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Time.Clock.System (SystemTime)
import System.Log.Bunyan
  ( Logger(..)
  , Priority(..)
  , consoleHandler
  , duration
  , intPriority
  , loggerNames'
  , noopHandler
  , priorityMap'
  , rootLogger
  )
import qualified System.Log.Bunyan as B

-- | Lens typeclass for getting a logger
class HasLogger a where
  logger :: Lens' a Logger

instance HasLogger Logger where
  logger = id

newtype LogRecord =
  LogRecord A.Object
  deriving (Show, Eq)

-- | The Bunyan Logging typeclass
--
-- Defines the logging primitives
--
-- Note that the root logger is not abstracted, you
-- have to create it with the IO implementation
class (HasLogger r, MonadReader r m, Monad m) =>
      MonadBunyan r m
  where
  childLogger :: T.Text -> A.Object -> m Logger
  -- ^ Create a child logger with the given name and default properties
  --
  -- At creation time, it will read shared config for the
  -- loglevel of the given name.  The decision to log or not
  -- is simply an integer comparison - so try to have long lived
  -- child loggers.
  getLoggingTime :: m SystemTime
  -- ^ Get the system time for logging
  --
  -- Log records contain the current timestamp
  handleRecord :: A.Object -> m () -- ^ Handle a finished log record via the root loggers handler
  --
  -- It has everything from it's loggers context
  -- and root context, also the time will be filled in.
  -- And of course, it has the message and priority
  -- of it's creation.

-- yes, it's an orphan instance, but i don't want to get more of the
-- implentations into the (supposedly abstract) Bunyan.Class module
instance HasLogger r => MonadBunyan r (ReaderT r IO) where
  childLogger n ctx = do
    lg <- asks (view logger)
    liftIO $ B.childLogger n ctx lg
  getLoggingTime = liftIO B.getLoggingTime
  handleRecord obj = do
    lg <- asks (view logger)
    liftIO $ B.handleRecord obj lg

-- | Log a message at level INFO - see logRecord for full API
logInfo :: MonadBunyan r m => T.Text -> m ()
logInfo = logRecord INFO M.empty

-- | Log a message at level DEBUG - see logRecord for full API
logDebug :: MonadBunyan r m => T.Text -> m ()
logDebug = logRecord DEBUG M.empty

-- | Log a message at level ERROR - see logRecord for full API
logError :: MonadBunyan r m => T.Text -> m ()
logError = logRecord ERROR M.empty

-- | Log a message at level WARN - see logRecord for full API
logWarn :: MonadBunyan r m => T.Text -> m ()
logWarn = logRecord WARN M.empty

-- | Log a message at level TRACE - see logRecord for full API
logTrace :: MonadBunyan r m => T.Text -> m ()
logTrace = logRecord TRACE M.empty

-- | Log the duration of the action.
logDuration :: MonadBunyan r m => m a -> m a
logDuration action = do
  start <- getLoggingTime
  a <- action
  end <- getLoggingTime
  uncurry (logRecord INFO) (duration start end)
  pure a

-- | Log a json record to the rootLoggers handler
logRecord :: MonadBunyan r m => Priority -> A.Object -> T.Text -> m ()
logRecord pri obj msg = do
  lg <- asks (view logger)
  let pri' = intPriority pri
  when (pri' >= priority lg) $ do
    tm <- getLoggingTime
    handleRecord (B.decorateRecord pri obj msg tm lg)

--- | Call the action with a local childlogger
localLogger :: MonadBunyan r m => T.Text -> A.Object -> m a -> m a
localLogger n ctx action = do
  lg <- childLogger n ctx
  local (over logger (const lg)) action
