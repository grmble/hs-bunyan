module BenchRIO where

import Control.Monad.Reader
import Criterion.Main (Benchmark, bench, nfIO)
import qualified Data.Aeson as A
import Data.Foldable (for_)
import Data.HashMap.Strict as M
import Data.Monoid ((<>))
import System.Log.Bunyan.LogText
import System.Log.Bunyan.RIO

type BRIO = ReaderT Logger IO

benchN :: Int -> Logger -> (Int -> BRIO ()) -> Benchmark
benchN n lg action =
  bench ("RIO " <> show n) $ nfIO $ runReaderT (for_ [1 .. n] action) lg

-- plain logRecord with context function
logWithContext :: Int -> BRIO ()
logWithContext i =
  logRecord DEBUG (M.insert "counter" (A.toJSON i)) ("test " <> toText (show i))

-- log with modified context
logModifyContext :: Int -> BRIO ()
logModifyContext i =
  local (modifyContext (M.insert "counter" (A.toJSON i))) $
  logRecord DEBUG id ("test " <> toText (show i))

-- log with a fresh named logger (i.e. shared config is read)
logWithLogger :: Int -> BRIO ()
logWithLogger i =
  withLogger
    (M.insert "counter" (A.toJSON i))
    (logRecord DEBUG id ("test " <> toText (show i)))
