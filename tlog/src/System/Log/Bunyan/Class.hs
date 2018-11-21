{-# LANGUAGE TemplateHaskell #-}

{- | Bunyan Logging Class

Defines the Bunyan Type class that is implemented for
IO and Free.
-}
module System.Log.Bunyan.Class
  ( module Data.Time.Clock.System
  , HasLogger(..)
  , Priority(..)
  , Logger(..)
  , LogRecord(..)
  , MonadBunyan(..)
  , intPriority
  ) where

import Control.Lens.PicoLens (Lens')
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as ATH
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import Data.Time.Clock.System (SystemTime)
import GHC.Generics
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

-- | A log record.
--
-- It has everything from it's loggers context
-- and root context, also the time will be filled in.
-- And of course, it has the message and priority
-- of it's creation.
newtype LogRecord =
  LogRecord AT.Object
  deriving (Show, Eq)

-- | The Bunyan Logging typeclass
--
-- Defines the logging primitives
--
-- Note that the root logger is not abstracted, you
-- have to create it with the IO implementation
class (HasLogger r, MonadReader r m, Monad m) => MonadBunyan r m where
  childLogger :: T.Text -> AT.Object -> m Logger
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
  handleRecord :: LogRecord -> m ()
  -- ^ Handle a finished log record via the root loggers handler
