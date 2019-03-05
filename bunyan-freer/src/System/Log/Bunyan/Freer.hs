{- | Bunyan Logging / FreerSimple

Note that WE ARE NOT USING the classy HasLogger lenses
- see https://github.com/lexi-lambda/freer-simple/issues/7

-}
module System.Log.Bunyan.Freer
  ( module System.Log.Bunyan
  , Bunyan(..)
  , namedLogger
  , withNamedLogger
  , withLogger
  , logRecord
  , logInfo
  , logDebug
  , logWarn
  , logError
  , logTrace
  , logDuration
  , thenLogDuration
  , runBunyan
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Time.Clock.System as SC
import System.Log.Bunyan (Logger, Priority(..), modifyContext, rootLogger)
import qualified System.Log.Bunyan as B

-- | Bunyan primitives for effect monad
data Bunyan x where
  NamedLogger :: T.Text -> (A.Object -> A.Object) -> Logger -> Bunyan Logger
  LogRecord
    :: Priority -> (A.Object -> A.Object) -> T.Text -> Logger -> Bunyan ()
  GetLoggingTime :: Bunyan SC.SystemTime

namedLogger ::
     Members '[ Reader Logger, Bunyan] effs
  => T.Text
  -> (A.Object -> A.Object)
  -> Eff effs Logger
namedLogger n f = ask >>= send . NamedLogger n f

logRecord ::
     Members '[ Reader Logger, Bunyan] effs
  => Priority
  -> (A.Object -> A.Object)
  -> T.Text
  -> Eff effs ()
logRecord pri ctx msg = ask >>= send . LogRecord pri ctx msg

getLoggingTime :: Member Bunyan effs => Eff effs SC.SystemTime
getLoggingTime = send GetLoggingTime

thenLogDuration ::
     Members '[ Reader Logger, Bunyan] effs
  => Eff effs a
  -> (a -> Eff effs Logger)
  -> Eff effs a
thenLogDuration action lgaction = do
  start <- getLoggingTime
  a <- action
  end <- getLoggingTime
  lg <- lgaction a
  local (const lg) $ uncurry (logRecord INFO) (B.duration start end)
  pure a

logDuration ::
     Members '[ Reader Logger, Bunyan] effs => Eff effs a -> Eff effs a
logDuration action = do
  lg <- ask
  thenLogDuration action (const $ pure lg)

withNamedLogger ::
     Members '[ Reader Logger, Bunyan] effs
  => T.Text
  -> (A.Object -> A.Object)
  -> Eff effs a
  -> Eff effs a
withNamedLogger n ctx action = do
  lg <- namedLogger n ctx
  local (const lg) action

withLogger ::
     Members '[ Reader Logger, Bunyan] effs
  => (A.Object -> A.Object)
  -> Eff effs a
  -> Eff effs a
withLogger ctx = local (modifyContext ctx)

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

runBunyan :: Eff '[ Bunyan, Reader Logger, IO] a -> Eff '[ Reader Logger, IO] a
runBunyan =
  interpretM
    (\case
       NamedLogger n f lg -> B.namedLogger n f lg
       LogRecord pri ctx msg lg -> B.logRecord pri ctx msg lg
       GetLoggingTime -> SC.getSystemTime)
