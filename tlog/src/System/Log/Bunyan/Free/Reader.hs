module System.Log.Bunyan.Free.Reader where

import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Reader
import Data.Functor (($>))
import Data.Functor.Sum (Sum(..))

-- | A Reader functor where
--
-- inner is the inner functor
-- r is the type in the reader
--
data ReaderF inner r x where
  AskF :: (r -> x) -> ReaderF inner r x
  LocalF :: Functor inner => (r -> r) -> WrapFree inner x -> ReaderF inner r x

instance Functor (ReaderF inner r) where
  fmap f (AskF fx) = AskF $ f . fx
  fmap f (LocalF fr body) = LocalF fr (f <$> body)

-- we need wrapped free so we can define the MonadReader instance
newtype WrapFree f a = WrapFree
  { unwrapFree :: Free f a
  } deriving (Functor, Applicative, Monad)

-- | An error functor
--
-- string errors for now ...
newtype ErrorF e x = ErrorF e deriving (Functor)

data PrintF x =
  PrintF String
         x
  deriving (Show, Eq, Functor)

-- cycle in type alias definition - so it has to be a newtype
newtype LogF e r x =
  LogF (Sum (ReaderF (LogF e r) r) (Sum (ErrorF e) PrintF) x)
  deriving (Functor)

instance MonadReader r (WrapFree (LogF e r)) where
  ask = WrapFree $ liftF $ LogF $ InL $ AskF id
  local rf body = WrapFree $ liftF $ LogF $ InL $ LocalF rf body

instance MonadError e (WrapFree (LogF e r)) where
  throwError e = WrapFree $ liftF $ LogF $ InR $ InL $ ErrorF e
  catchError = undefined

interpretReader ::
     MonadReader r m => (forall a. inner a -> m a) -> ReaderF inner r x -> m x
interpretReader _ (AskF rx) = reader rx
interpretReader interpret (LocalF fr body) =
  local fr (foldFree interpret (unwrapFree body))

interpretError :: MonadError e m => ErrorF e x -> m x
interpretError (ErrorF msg) = throwError msg

interpretPrint :: MonadIO m => PrintF x -> m x
interpretPrint (PrintF s x) = liftIO (putStrLn s) $> x

interpretLog ::
     (MonadIO m, MonadError e m, MonadReader r m) => LogF e r x -> m x
interpretLog (LogF (InL logreader)) = interpretReader interpretLog logreader
interpretLog (LogF (InR (InL err))) = interpretError err
interpretLog (LogF (InR (InR printer))) = interpretPrint printer

xPrint :: String -> WrapFree (LogF e inner) ()
xPrint s = WrapFree $ liftF $ LogF $ InR $ InR $ PrintF s ()

testLog :: WrapFree (LogF IOError Int) ()
testLog = do
  i1 <- ask
  xPrint (show (i1 :: Int))
  local (+ 1) $ do
    i2 <- ask
    xPrint (show (i2 :: Int))
  i3 <- ask
  _ <- throwError $ userError "blubb"
  xPrint (show (i3 :: Int))

xxx :: IO ()
xxx = runReaderT (foldFree interpretLog (unwrapFree testLog)) 1
