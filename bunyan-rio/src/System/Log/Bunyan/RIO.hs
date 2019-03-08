{- | Bunyan RIO Implementation

Offers the API from "System.Log.Bunyan" in an MTL friendly
way.  Please check that module for documentation.

-}
module System.Log.Bunyan.RIO
  ( module System.Log.Bunyan
  , module Data.Time.Clock.System
  , HasLogger(..)
  , Bunyan
  , logInfo
  , logDebug
  , logError
  , logWarn
  , logTrace
  , logDuration
  , logDuration'
  , logRecord
  , namedLogger
  , withNamedLogger
  , withLogger
  ) where

import Control.Lens.PicoLens (Lens', over, view)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time.Clock.System (SystemTime, getSystemTime)
import System.Log.Bunyan
  ( Logger
  , Priority(..)
  , consoleHandler
  , modifyContext
  , noopHandler
  , rootLogger
  )
import qualified System.Log.Bunyan as B
import qualified System.Log.Bunyan.Types as B

-- | Lens typeclass for getting a logger
class HasLogger a where
  logger :: Lens' a Logger

instance HasLogger Logger where
  logger = id

type Bunyan r m = (HasLogger r, MonadReader r m, MonadIO m)

logInfo :: Bunyan r m => T.Text -> m ()
logInfo = logRecord INFO id

logDebug :: Bunyan r m => T.Text -> m ()
logDebug = logRecord DEBUG id

logError :: Bunyan r m => T.Text -> m ()
logError = logRecord ERROR id

logWarn :: Bunyan r m => T.Text -> m ()
logWarn = logRecord WARN id

logTrace :: Bunyan r m => T.Text -> m ()
logTrace = logRecord TRACE id

logRecord :: Bunyan r m => Priority -> (A.Object -> A.Object) -> T.Text -> m ()
logRecord pri fn msg = asks (view logger) >>= B.logRecord pri fn msg

logDuration :: Bunyan r m => m a -> m a
logDuration action = asks (view logger) >>= B.logDuration (const action)

logDuration' :: Bunyan r m => ((A.Object -> m ()) -> m a) -> m a
logDuration' action = asks (view logger) >>= B.logDuration' (\cb _ -> action cb)

namedLogger :: Bunyan r m => T.Text -> (A.Object -> A.Object) -> m Logger
namedLogger n f = asks (view logger) >>= B.namedLogger n f

withNamedLogger :: Bunyan r m => T.Text -> (A.Object -> A.Object) -> m a -> m a
withNamedLogger n f action = do
  lg <- namedLogger n f
  local (over logger (const lg)) action

withLogger :: Bunyan r m => (A.Object -> A.Object) -> m a -> m a
withLogger f action = do
  n <- asks (B.name . view logger)
  withNamedLogger n f action
