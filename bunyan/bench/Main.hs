module Main where

import Control.Lens.PicoLens
import Criterion.Main
import qualified Data.Aeson as A
import Data.Foldable (for_)
import Data.HashMap.Strict as M
import Data.Monoid ((<>))
import System.IO
import System.Log.Bunyan
import System.Log.Bunyan.LogText
import System.Log.Bunyan.Types
import UnliftIO.MVar
import UnliftIO.STM

logAtDebug :: Int -> Logger -> IO ()
logAtDebug n lg =
  for_ (take n $ iterate succ (0 :: Int)) $ \i ->
    logRecord
      DEBUG
      (M.insert "counter" (A.toJSON i))
      ("test " <> toText (show i))
      lg

logAtDebugWithLogger :: Int -> Logger -> IO ()
logAtDebugWithLogger n lg =
  for_ (take n $ iterate succ (0 :: Int)) $ \i ->
    withLogger
      (M.insert "counter" (A.toJSON i))
      (logRecord DEBUG id ("test " <> toText (show i)))
      lg

logAtDebugModifyContext :: Int -> Logger -> IO ()
logAtDebugModifyContext n lg =
  for_ (take n $ iterate succ (0 :: Int)) $ \i ->
    (logRecord DEBUG id ("test " <> toText (show i)) .
     (modifyContext (M.insert "counter" (A.toJSON i))))
      lg

main :: IO ()
main = do
  logvar <- newEmptyMVar
  withLogWriter logvar "bench.log" $ do
    rl <- rootLogger "root" INFO (putMVar logvar)
    atomically $ modifyTVar (view priorityMap rl) (M.insert "child" DEBUG)
    dbg <- namedLogger "child" id rl
    defaultMain
      [ bgroup
          "logAtDebug"
          [ bench "1000" $ nfIO (logAtDebug 1000 rl)
          , bench "10000" $ nfIO (logAtDebug 10000 rl)
          , bench "100000" $ nfIO (logAtDebug 100000 rl)
          , bench "1000000" $ nfIO (logAtDebug 1000000 rl)
          ]
      , bgroup
          "logAtDebugWithLogger"
          [ bench "1000" $ nfIO (logAtDebugWithLogger 1000 rl)
          , bench "10000" $ nfIO (logAtDebugWithLogger 10000 rl)
          , bench "100000" $ nfIO (logAtDebugWithLogger 100000 rl)
          , bench "1000000" $ nfIO (logAtDebugWithLogger 1000000 rl)
          ]
      , bgroup
          "logAtDebugModifyContext"
          [ bench "1000" $ nfIO (logAtDebugModifyContext 1000 rl)
          , bench "10000" $ nfIO (logAtDebugModifyContext 10000 rl)
          , bench "100000" $ nfIO (logAtDebugModifyContext 100000 rl)
          , bench "1000000" $ nfIO (logAtDebugModifyContext 1000000 rl)
          ]
      , bgroup
          "logAtDebug LOGGING"
          [ bench "1000" $ nfIO (logAtDebug 1000 dbg)
          , bench "10000" $ nfIO (logAtDebug 10000 dbg)
          ]
      , bgroup
          "logAtDebugWithLogger LOGGING"
          [ bench "1000" $ nfIO (logAtDebugWithLogger 1000 dbg)
          , bench "10000" $ nfIO (logAtDebugWithLogger 10000 dbg)
          ]
      ]
