{- | Bunyan Logging / Free

A structured logging package like node's bunyan.
The output is compatible with bunyan.


-}
module System.Log.Bunyan.Free
  ( module System.Log.Bunyan
  , BunyanF(..)
  , HasSystemTime(..)
  , MonadBunyan(..)
  , localLogger
  , logDebug
  , logError
  , logInfo
  , logDuration
  , logTrace
  , logWarn
  , interpretIO
  ) where

import Control.Lens.PicoLens (Lens', over)
import Control.Monad.Free.Church
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.Functor (($>))
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Time.Clock.System as C
import System.Log.Bunyan hiding
  ( childLogger
  , getSystemTime
  , localLogger
  , logDebug
  , logDuration
  , logError
  , logInfo
  , logRecord
  , logTrace
  , logWarn
  )
import qualified System.Log.Bunyan as B

-- | MonadBunyan - primitives for Bunyan free logging
--
-- Note that the root logger has to be created from IO.
class (HasLogger r, MonadReader r m, Monad m) =>
      MonadBunyan r m
  where
  childLogger :: T.Text -> A.Object -> m Logger
  -- ^ Create a child logger
  -- | Log a generic log record.
  --
  -- It takes a priority, context object and message.
  logRecord :: Priority -> A.Object -> T.Text -> m ()
  -- ^ Logging primitive, you probably want logInfo, logDebug, ...
  getSystemTime :: m SystemTime -- ^ Get the current time.
  --
  -- This simply wraps getSystemTime and is needed for the duration helpers

data BunyanF x
  = ChildLoggerF T.Text
                 A.Object
                 (Logger -> x)
  | LogRecordF Priority
               A.Object
               T.Text
               x
  | SystemTimeF (SystemTime -> x)
  deriving (Functor)

localLogger :: MonadBunyan r m => T.Text -> A.Object -> m a -> m a
localLogger n ctx action = do
  lg <- childLogger n ctx
  local (over logger (const lg)) action

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
  start <- getSystemTime
  a <- action
  end <- getSystemTime
  uncurry (logRecord INFO) (duration start end)
  pure a

-- | Interpret the bunyan DSL in a reader/io
interpretIO :: (HasLogger r, MonadReader r m, MonadIO m) => BunyanF a -> m a
interpretIO (ChildLoggerF n ctx f) = f <$> B.childLogger n ctx
interpretIO (LogRecordF pri ctx msg x) = B.logRecord pri ctx msg $> x
interpretIO (SystemTimeF f) = f <$> liftIO C.getSystemTime

-- | Base MonadBunyan instance
instance HasLogger r => (MonadBunyan r) (ReaderT r (F BunyanF)) where
  childLogger n ctx = liftF $ ChildLoggerF n ctx id
  logRecord pri ctx msg = liftF $ LogRecordF pri ctx msg ()
  getSystemTime = liftF $ SystemTimeF id

-- | System time reader env for testing
class HasSystemTime a where
  fixedSystemTime :: Lens' a SystemTime

instance HasSystemTime SystemTime where
  fixedSystemTime = id
