module Control.Monad.Free.Trans where

import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Reader

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

-- | a free monad error
--
-- we don't use the real one because we don't want the handling capabilities
class MonadFreeError e m | m -> e where
  freeError :: e -> m a

interpretReader ::
     MonadReader r m => (forall a. inner a -> m a) -> ReaderF inner r x -> m x
interpretReader _ (AskF rx) = reader rx
interpretReader interpret (LocalF fr body) =
  local fr (foldFree interpret (unwrapFree body))

interpretError :: MonadError e m => ErrorF e x -> m x
interpretError (ErrorF msg) = throwError msg
