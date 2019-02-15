{-# LANGUAGE TemplateHaskell #-}

{- | Bunyan Logging Class

Defines the Bunyan Type class that is implemented for
IO and Free.
-}
module System.Log.Bunyan.Class
  ( HasLogger(..)
  , MonadBunyan(..)
  ) where

import Control.Lens.PicoLens (Lens')
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time.Clock.System (SystemTime)
import System.Log.Bunyan (Logger(..))

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
