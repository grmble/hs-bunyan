module System.Log.Bunyan.FreeTrans where

import Control.Monad.Except (MonadError(..))
import Control.Monad.Free.Church (F(..), foldF)
import Control.Monad.Reader (MonadReader(..))

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
--
-- XXX: ideally,  we could make this work for any MonadFree
-- if we can't, we use the church encoding
newtype WrapFree f a = WrapFree
  { unwrapFree :: F f a
  } deriving (Functor, Applicative, Monad)

-- | An error functor
newtype ErrorF e x =
  ErrorF e
  deriving (Functor)

-- | a free monad error
--
-- we don't use the real one because we don't want the handling capabilities
class MonadFreeError e m | m -> e where
  freeError :: e -> m a

interpretReader ::
     MonadReader r m => (forall a. inner a -> m a) -> ReaderF inner r x -> m x
interpretReader _ (AskF rx) = reader rx
interpretReader interpret (LocalF fr body) =
  local fr (foldF interpret (unwrapFree body))

interpretError :: MonadError e m => ErrorF e x -> m x
interpretError (ErrorF msg) = throwError msg
