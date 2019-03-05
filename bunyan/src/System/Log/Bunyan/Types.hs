{-# LANGUAGE TemplateHaskell #-}

{- | Bunyan type definitions

Extra module so main Bunyan module can hide record accessors,
while still being exported from here.

-}
module System.Log.Bunyan.Types
  ( Logger(..)
  , Priority(..)
  , intPriority
  , textToPriority
  , loggerNames
  , priorityMap
  ) where

import Control.Lens.PicoLens (Lens')
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as ATH
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import GHC.Generics (Generic)
import UnliftIO.STM (TVar)

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

-- | Get the priority from Text
--
-- For values not in the type, INFO is used.
textToPriority :: T.Text -> Priority
textToPriority "FATAL" = FATAL
textToPriority "ERROR" = ERROR
textToPriority "WARN" = WARN
textToPriority "INFO" = INFO
textToPriority "DEBUG" = DEBUG
textToPriority "TRACE" = TRACE
textToPriority _ = INFO


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
  , context :: {-# UNPACK #-}!A.Object
    -- ^ current context object - can be overwritten
  , priority :: {-# UNPACK #-}!Int
    -- ^ log level of the logger
  , handler :: {-# UNPACK #-}!(A.Object -> IO ())
    -- ^ handler for log records
  , rootContext :: {-# UNPACK #-}!A.Object
    -- ^ root context - coKntains everything that can be determined
    -- when the root logger is created.  Cant be overwritten.
  , _priorityMap :: {-# UNPACK #-}!(TVar (M.HashMap T.Text Priority))
    -- ^ map of logger names to priority
  , _loggerNames :: {-# UNPACK #-}!(TVar (S.HashSet T.Text))
  }

instance Show Logger where
  show lg =
    "Logger name=" ++
    show (name lg) ++
    "\n    context=" ++
    show (context lg) ++
    "\n    rootContext=" ++
    show (rootContext lg) ++ "\n    priority=" ++ show (priority lg)

loggerNames :: Lens' Logger (TVar (S.HashSet T.Text))
loggerNames k lg = fmap (\x -> lg {_loggerNames = x}) (k (_loggerNames lg))

priorityMap :: Lens' Logger (TVar (M.HashMap T.Text Priority))
priorityMap k lg = fmap (\x -> lg {_priorityMap = x}) (k (_priorityMap lg))
