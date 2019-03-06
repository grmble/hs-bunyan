module BenchFreer where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Criterion.Main (Benchmark, bench, nfIO)
import qualified Data.Aeson as A
import Data.Foldable (for_)
import Data.HashMap.Strict as M
import Data.Monoid ((<>))
import System.Log.Bunyan.Freer
import System.Log.Bunyan.LogText

benchN ::
     Int -> Logger -> (Int -> Eff '[ Bunyan, Reader Logger, IO] ()) -> Benchmark
benchN n lg action =
  bench ("Freer " <> show n) $
  nfIO $ runM $ runReader lg $ runBunyan (for_ [1 .. n] action)

-- plain logRecord with context function
logWithContext :: Members '[ Reader Logger, Bunyan] effs => Int -> Eff effs ()
logWithContext i =
  logRecord DEBUG (M.insert "counter" (A.toJSON i)) ("test " <> toText (show i))

-- log with modified context
logModifyContext :: Members '[ Reader Logger, Bunyan] effs => Int -> Eff effs ()
logModifyContext i =
  local (modifyContext (M.insert "counter" (A.toJSON i))) $
  logRecord DEBUG id ("test " <> toText (show i))

-- log with a fresh named logger (i.e. shared config is read)
logWithLogger :: Members '[ Reader Logger, Bunyan] effs => Int -> Eff effs ()
logWithLogger i =
  withLogger
    (M.insert "counter" (A.toJSON i))
    (logRecord DEBUG id ("test " <> toText (show i)))
