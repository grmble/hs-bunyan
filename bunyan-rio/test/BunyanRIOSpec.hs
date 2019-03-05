module BunyanRIOSpec where

import Control.Lens.PicoLens
import Control.Monad.Reader
import qualified Data.HashMap.Strict as M
import Test.Hspec
import System.Log.Bunyan.RIO
import System.Log.Bunyan.Types
import UnliftIO.IORef
import UnliftIO.STM

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "check log levels" $ do
    it "rootlogger info" $ do
      var <- newIORef []
      rl <- rootLogger "root" INFO (handler var)
      _ <- runReaderT ioAction rl
      records <- readIORef var
      length records `shouldBe` 2
    it "rootlogger debug" $ do
      var <- newIORef []
      rl <- rootLogger "root" DEBUG (handler var)
      _ <- runReaderT ioAction rl
      records <- readIORef var
      length records `shouldBe` 4
    it "rootlogger error" $ do
      var <- newIORef []
      rl <- rootLogger "root" ERROR (handler var)
      _ <- runReaderT ioAction rl
      records <- readIORef var
      length records `shouldBe` 0
    it "rootlogger info/childLogger debug" $ do
      var <- newIORef []
      rl <- rootLogger "root" INFO (handler var)
      atomically $ modifyTVar (view priorityMap rl) (M.insert "child" DEBUG)
      _ <- runReaderT ioAction rl
      records <- readIORef var
      length records `shouldBe` 3
  where
    handler var logrec = modifyIORef var (logrec :)

ioAction :: ReaderT Logger IO ()
ioAction = do
  logInfo "info@root"
  logDebug "debug@root"
  withNamedLogger "child" (M.insert "x" "17") $ do
    logInfo "info@child"
    logDebug "debug@child"
