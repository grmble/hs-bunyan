module Main where

import qualified BenchBunyan as BB
import qualified BenchRIO as BR
import qualified BenchFreer as BF
import Control.Lens.PicoLens
import Criterion.Main
import Data.HashMap.Strict as M
import System.Log.Bunyan
import System.Log.Bunyan.Types
import UnliftIO.MVar
import UnliftIO.STM

main :: IO ()
main = do
  let count = 100000
  -- let outputCount = 20000
  logvar <- newEmptyMVar
  withLogWriter logvar "bench.log" $ do
    root <- rootLogger "root" INFO (putMVar logvar)
    atomically $ modifyTVar (view priorityMap root) (M.insert "child" DEBUG)
    -- child <- namedLogger "child" id root
    defaultMain
      [ bgroup
          "logWithContext"
          [ BB.benchN count $ BB.logWithContext root
          , BR.benchN count root BR.logWithContext
          , BF.benchN count root BF.logWithContext
          ]
      , bgroup
          "logModifyContext"
          [ BB.benchN count $ BB.logModifyContext root
          , BR.benchN count root BR.logModifyContext
          , BF.benchN count root BF.logModifyContext
          ]
      , bgroup
          "logWithLogger"
          [ BB.benchN count $ BB.logWithLogger root
          , BR.benchN count root BR.logWithLogger
          , BF.benchN count root BF.logWithLogger
          ]
      {--

      Output completely domintes these benchmarks
      by a factor of 50 or so.  Nothing to be seen here.

      , bgroup
          "logWithContext (with output)"
          [ BB.benchN outputCount $ BB.logWithContext child
          , BR.benchN outputCount child BR.logWithContext
          ]
      , bgroup
          "logModifyContext (with output)"
          [ BB.benchN outputCount $ BB.logModifyContext child
          , BR.benchN outputCount child BR.logModifyContext
          ]
      , bgroup
          "logWithLogger (with output)"
          [ BB.benchN outputCount $ BB.logWithLogger child
          , BR.benchN outputCount child BR.logWithLogger
        ]
      --}
      ]
