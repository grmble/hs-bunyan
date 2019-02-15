module BunyanFreerSpec where

import Control.Lens.PicoLens
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import qualified Data.HashMap.Strict as M
import Data.Maybe (isJust)
import qualified Data.Time.Clock.System as SC
import System.Log.Bunyan.Freer
import qualified System.Log.Bunyan as B
import Test.Hspec
import UnliftIO.IORef
import UnliftIO.STM

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "check log levels xxx" $ do
    it "rootlogger info" $ do
      var <- newIORef []
      rl <- rootLogger "root" INFO (handler var)
      _ <- ioAction rl
      records <- readIORef var
      length records `shouldBe` 2
    it "rootlogger debug" $ do
      var <- newIORef []
      rl <- B.rootLogger "root" DEBUG (handler var)
      _ <- ioAction rl
      records <- readIORef var
      length records `shouldBe` 4
    it "rootlogger error" $ do
      var <- newIORef []
      rl <- B.rootLogger "root" ERROR (handler var)
      _ <- ioAction rl
      records <- readIORef var
      length records `shouldBe` 0
    it "rootlogger info/childLogger debug" $ do
      var <- newIORef []
      rl <- B.rootLogger "root" INFO (handler var)
      atomically $ modifyTVar (view B.priorityMap' rl) (M.insert "child" DEBUG)
      _ <- ioAction rl
      records <- readIORef var
      length records `shouldBe` 3
  describe "check duration helper" $
    it "should compute suitable headers" $ do
      (ctx, msg) <- duration <$> SC.getSystemTime <*> SC.getSystemTime
      show msg `shouldContain` "completed in"
      ctx `shouldSatisfy` (isJust . M.lookup "duration")
  where
    handler var logrec = modifyIORef var (logrec :)

ioAction :: Logger -> IO SystemTime
ioAction lg = runM $ runBunyan lg dslAction

dslAction :: Eff '[Bunyan, Reader Logger, IO] SystemTime
dslAction = do
  logInfo "info@root"
  logDebug "debug@root"
  localLogger "child" (M.singleton "x" "17") $ do
    logInfo "info@child"
    logDebug "debug@child"
    getLoggingTime
