module BunyanFreeSpec where

import Control.Lens.PicoLens
import Control.Monad.Free
import Control.Monad.Reader
import Data.Functor.Sum
import qualified Data.HashMap.Strict as M
import Data.Maybe (isJust)
import qualified Data.Time.Clock.System as SC
import System.Log.Bunyan.Free
import System.Log.Bunyan.FreeTrans
import System.Log.Bunyan.IO
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
      _ <- runReaderT ioAction rl
      records <- readIORef var
      length records `shouldBe` 3
  describe "check duration helper" $
    it "should compute suitable headers" $ do
      (ctx, msg) <- duration <$> SC.getSystemTime <*> SC.getSystemTime
      show msg `shouldContain` "completed in"
      ctx `shouldSatisfy` (isJust . M.lookup "duration")
  where
    handler var logrec = modifyIORef var (logrec :)

ioAction :: ReaderT Logger IO SystemTime
ioAction = foldFree interpretDsl (unwrapFree dslAction)

{-- for future reference in case F makes a comeback
foldReaderF ::
     Monad m
  => (forall x. f x -> ReaderT r m x)
  -> ReaderT r (F f) a
  -> ReaderT r m a
foldReaderF interpret dsl =
  ReaderT $ \r -> runReaderT (foldF interpret (runReaderT dsl r)) r
 --}
-- cycle in type alias definition - so it has to be a newtype
newtype DslF r x
  -- you probably want (Sum BunyanF YourFunctor)
         =
  DslF (Sum (ReaderF (DslF r) r) BunyanF x)
  deriving (Functor)

instance MonadReader r (WrapFree (DslF r)) where
  ask = WrapFree $ liftF $ DslF $ InL $ AskF id
  local rf body = WrapFree $ liftF $ DslF $ InL $ LocalF rf body

instance HasLogger r => MonadBunyan r (WrapFree (DslF r)) where
  childLogger n ctx = WrapFree $ liftF $ DslF $ InR $ ChildLoggerF n ctx id
  getLoggingTime = WrapFree $ liftF $ DslF $ InR $ LoggingTimeF id
  handleRecord obj = WrapFree $ liftF $ DslF $ InR $ HandleRecordF obj ()

dslAction :: WrapFree (DslF Logger) SystemTime
dslAction = do
  logInfo "info@root"
  logDebug "debug@root"
  localLogger "child" (M.singleton "x" "17") $ do
    logInfo "info@child"
    logDebug "debug@child"
    getLoggingTime

interpretDsl :: HasLogger r => DslF r x -> ReaderT r IO x
interpretDsl (DslF (InL logreader)) = interpretReader interpretDsl logreader
interpretDsl (DslF (InR bunyan)) = interpretBunyanIO bunyan
