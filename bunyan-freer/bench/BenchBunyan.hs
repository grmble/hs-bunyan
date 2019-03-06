module BenchBunyan where

import Criterion.Main (Benchmark, bench, nfIO)
import qualified Data.Aeson as A
import Data.Foldable (for_)
import Data.HashMap.Strict as M
import Data.Monoid ((<>))
import System.Log.Bunyan
import System.Log.Bunyan.LogText


benchN :: Int -> (Int -> IO ()) -> Benchmark
benchN n action =
  bench ("plain " <> show n) $ nfIO (for_ [1..n] action)


-- plain logRecord with context function
logWithContext :: Logger -> Int -> IO ()
logWithContext lg i =
  logRecord
    DEBUG
    (M.insert "counter" (A.toJSON i))
    ("test " <> toText (show i))
    lg

-- log with modified context
logModifyContext :: Logger -> Int -> IO ()
logModifyContext lg i =
  logRecord
    DEBUG
    id
    ("test " <> toText (show i))
    (modifyContext (M.insert "counter" (A.toJSON i)) lg)

-- log with a fresh named logger (i.e. shared config is read)
logWithLogger :: Logger -> Int -> IO ()
logWithLogger lg i =
  withLogger
    (M.insert "counter" (A.toJSON i))
    (logRecord DEBUG id ("test " <> toText (show i)))
    lg
