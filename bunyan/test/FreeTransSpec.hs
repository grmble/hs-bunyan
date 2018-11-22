module FreeTransSpec where

import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Reader
import Data.Functor (($>))
import Data.Functor.Sum
import System.Log.Bunyan.FreeTrans
import Test.Hspec
import UnliftIO.IORef

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "checking free reader" $
    it "increased value in local" $ do
      acc <- newIORef []
      runReaderT (foldFree (interpretLog acc) (unwrapFree incAction)) 1
      records <- readIORef acc
      reverse records `shouldBe` [1, 2, 1]
  describe "checking free error" $
    it "should throw after local block" $ do
      acc <- newIORef []
      catchError
        (runReaderT (foldFree (interpretLog acc) (unwrapFree throwAction)) 1)
        (const $ pure ())
      records <- readIORef acc
      reverse records `shouldBe` [1, 2]

commonAction :: WrapFree (LogF IOError Int) ()
commonAction = do
  i1 <- ask
  xPrint i1
  local (+ 1) $ do
    i2 <- ask
    xPrint i2

incAction :: WrapFree (LogF IOError Int) ()
incAction = do
  commonAction
  i3 <- ask
  xPrint i3

throwAction :: WrapFree (LogF IOError Int) ()
throwAction = do
  commonAction
  _ <- freeError $ userError "i don't want to do this anymore"
  i3 <- ask
  xPrint i3

data PrintF x =
  PrintF Int
         x
  deriving (Show, Eq, Functor)

-- cycle in type alias definition - so it has to be a newtype
newtype LogF e r x =
  LogF (Sum (ReaderF (LogF e r) r) (Sum (ErrorF e) PrintF) x)
  deriving (Functor)

instance MonadReader r (WrapFree (LogF e r)) where
  ask = WrapFree $ liftF $ LogF $ InL $ AskF id
  local rf body = WrapFree $ liftF $ LogF $ InL $ LocalF rf body

instance MonadFreeError e (WrapFree (LogF e r)) where
  freeError e = WrapFree $ liftF $ LogF $ InR $ InL $ ErrorF e

interpretPrint :: MonadIO m => IORef [Int] -> PrintF x -> m x
interpretPrint acc (PrintF i x) = liftIO (modifyIORef acc (i :)) $> x

interpretLog ::
     (MonadIO m, MonadError e m, MonadReader r m)
  => IORef [Int]
  -> LogF e r x
  -> m x
interpretLog acc (LogF (InL logreader)) =
  interpretReader (interpretLog acc) logreader
interpretLog _ (LogF (InR (InL err))) = interpretError err
interpretLog acc (LogF (InR (InR printer))) = interpretPrint acc printer

xPrint :: Int -> WrapFree (LogF e inner) ()
xPrint i = WrapFree $ liftF $ LogF $ InR $ InR $ PrintF i ()
