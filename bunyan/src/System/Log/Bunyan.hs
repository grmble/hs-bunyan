{- | Bunyan Logging

A structured logging package like node's bunyan.
The output is compatible with bunyan.

This defines the API in terms of MonadBunyan from Bunyan.Class

At some point you will have to import the IO implementation,
for the IO and Free implementation (because you will interpret
the Free implementation into IO)
-}
module System.Log.Bunyan
  ( module System.Log.Bunyan.Class
  , module Data.Time.Clock.System
  , localLogger
  , logRecord
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logTrace
  , logDuration
  , duration
  ) where

import Control.Lens.PicoLens (over, view)
import Control.Monad (when)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Time.Clock.System (SystemTime, getSystemTime)
import qualified Data.Time.Clock.System as C
import System.Log.Bunyan.Class
import Text.Printf (printf)
import Text.Show.Functions ()

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

-- | Helper to compute a duration.
--
-- This will compute a suitable context object and message
-- which can be logged using
--
--    uncurry
--      (logRecord DEBUG) \
--      (duration <$> getSystemTime <*> getSystemTime)
duration :: SystemTime -> SystemTime -> (A.Object, T.Text)
duration start end = do
  let dur = double end - double start
  let ctx = M.singleton "duration" (A.Number $ Scientific.fromFloatDigits dur)
  let msg = T.pack $ printf "completed in %dms" ((round $ 1000 * dur) :: Int)
  (ctx, msg)
  where
    double :: SystemTime -> Double
    double sc =
      fromIntegral (C.systemSeconds sc) +
      fromIntegral (C.systemNanoseconds sc) / 1e9

-- | Call the action with a local childlogger
localLogger ::
     MonadBunyan r m => T.Text -> A.Object -> m a -> m a
localLogger n ctx action = do
  lg <- childLogger n ctx
  local (over logger (const lg)) action

-- | Log a json record to the rootLoggers handler
logRecord ::
     MonadBunyan r m => Priority -> A.Object -> T.Text -> m ()
logRecord pri obj msg = do
  lg <- asks (view logger)
  let pri' = intPriority pri
  when (pri' >= priority lg) $ do
    tm <- getLoggingTime
    handleRecord (decorate lg tm (context lg))
  where
    decorate lg t =
      M.insert "name" (A.String $ name lg) .
      M.insert "level" (A.Number $ fromIntegral $ intPriority pri) .
      M.insert "msg" (A.String msg) .
      M.union obj .
      M.insert "time" (A.toJSON $ C.systemToUTCTime t) .
      M.union (rootContext lg)
