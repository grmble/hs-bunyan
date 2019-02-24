{--# LANGUAGE AllowAmbiguousTypes #--}
{- | Bunyan Logging / FreerSimple

Note that WE ARE NOT USING the classy HasLogger lenses
- see https://github.com/lexi-lambda/freer-simple/issues/7

-}
module System.Log.Bunyan.Freer
  ( module System.Log.Bunyan
  , Bunyan(..)
  , childLogger
  , handleRecord
  , getLoggingTime
  , localLogger
  , logRecord
  , logInfo
  , logDebug
  , logWarn
  , logError
  , logTrace
  , logDuration
  , runBunyan
  ) where

import Control.Monad (when)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import qualified Data.Aeson as A
import qualified Data.Text as T
import System.Log.Bunyan
  ( Logger(..)
  , Priority(..)
  , SystemTime
  , decorateRecord
  , duration
  , intPriority
  , modifyContext
  , rootLogger
  )
import System.Log.Bunyan.LogText (LogText(..))
import qualified System.Log.Bunyan as B

-- | Bunyan primitives for effect monad
data Bunyan x where
  ChildLogger :: T.Text -> Logger -> Bunyan Logger
  HandleRecord :: A.Object -> Logger -> Bunyan ()
  LoggingTime :: Bunyan SystemTime

childLogger ::
     Members '[ Reader Logger, Bunyan] effs
  => T.Text
  -> Eff effs Logger
childLogger n = ask >>= send . ChildLogger n

handleRecord ::
     Members '[ Reader Logger, Bunyan] effs => A.Object -> Eff effs ()
handleRecord ctx = ask >>= send . HandleRecord ctx

getLoggingTime :: Member Bunyan effs => Eff effs SystemTime
getLoggingTime = send LoggingTime

localLogger ::
     Members '[ Reader Logger, Bunyan] effs
  => T.Text
  -> (A.Object -> A.Object)
  -> Eff effs a
  -> Eff effs a
localLogger n f action = do
  lg <- modifyContext f <$> childLogger n
  local (const lg) action

logRecord ::
     (LogText a, Member (Reader Logger) effs, Member Bunyan effs)
  => Priority
  -> (A.Object -> A.Object)
  -> a
  -> Eff effs ()
logRecord pri fn msg = do
  lg <- ask
  let pri' = intPriority pri
  when (pri' >= priority lg) $ do
    tm <- getLoggingTime
    handleRecord (decorateRecord pri fn msg tm lg)

logInfo :: Members '[ Bunyan, Reader Logger] effs => T.Text -> Eff effs ()
logInfo = logRecord INFO id

logDebug :: Members '[ Bunyan, Reader Logger] effs => T.Text -> Eff effs ()
logDebug = logRecord DEBUG id

logError :: Members '[ Bunyan, Reader Logger] effs => T.Text -> Eff effs ()
logError = logRecord ERROR id

logWarn :: Members '[ Bunyan, Reader Logger] effs => T.Text -> Eff effs ()
logWarn = logRecord WARN id

logTrace :: Members '[ Bunyan, Reader Logger] effs => T.Text -> Eff effs ()
logTrace = logRecord TRACE id

logDuration ::
     Members '[ Bunyan, Reader Logger] effs => Eff effs a -> Eff effs a
logDuration action = do
  start <- getLoggingTime
  a <- action
  end <- getLoggingTime
  uncurry (logRecord INFO) (duration start end)
  pure a

runBunyan :: Logger -> Eff '[ Bunyan, Reader Logger, IO] a -> Eff '[ IO] a
runBunyan lg =
  runReader lg .
  interpretM
    (\case
       ChildLogger n lg' -> B.childLogger n lg'
       HandleRecord obj lg' -> B.handleRecord obj lg'
       LoggingTime -> B.getLoggingTime)
