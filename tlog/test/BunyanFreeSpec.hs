module BunyanFreeSpec where

import Control.Lens.PicoLens
import Control.Monad.Free.Church
import Control.Monad.Reader
import qualified Data.HashMap.Strict as M
import Data.Maybe (isJust)
import qualified Data.Time.Clock.System as SC
import System.Log.Bunyan.Free
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
      atomically $ modifyTVar (view priorityMap' rl) (M.insert "child" DEBUG)
      x <- readTVarIO (view priorityMap' rl)
      print x
      _ <- runReaderT ioAction rl
      records <- readIORef var
      print records
      length records `shouldBe` 3
  describe "check duration helper" $
    it "should compute suitable headers" $ do
      (ctx, msg) <- duration <$> SC.getSystemTime <*> SC.getSystemTime
      show msg `shouldContain` "completed in"
      ctx `shouldSatisfy` (isJust . M.lookup "duration")
  where
    handler var logrec = modifyIORef var (logrec :)

ioAction :: ReaderT Logger IO SystemTime
ioAction = foldReaderF interpretIO dslAction

foldReaderF ::
     Monad m
  => (forall x. f x -> ReaderT r m x)
  -> ReaderT r (F f) a
  -> ReaderT r m a
foldReaderF interpret dsl =
  ReaderT $ \r -> runReaderT (foldF interpret (runReaderT dsl r)) r

dslAction :: ReaderT Logger (F BunyanF) SystemTime
dslAction = do
  logInfo "info@root"
  logDebug "debug@root"
  localLogger "child" (M.singleton "x" "17") $ do
    logInfo "info@child"
    logDebug "debug@child"
    getSystemTime
