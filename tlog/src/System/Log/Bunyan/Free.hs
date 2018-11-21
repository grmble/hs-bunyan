{- | Bunyan Logging / Free

Functor and interpreter for a Free Bunyan implementation.

Note that this does not provide the MonadBunyan instance
since you need to know the final Functor for the Free instance.

Hint: Sum (ReaderF YourNewtype) (Sum BunyanF YourAdditionalFunctor))
See FreeTransSpec.hs for simplest possible example.
-}
module System.Log.Bunyan.Free
  ( module System.Log.Bunyan
  , BunyanF(..)
  , interpretBunyanIO
  ) where

import qualified Data.Aeson as A
import Data.Functor (($>))
import qualified Data.Text as T
import System.Log.Bunyan

data BunyanF x
  = ChildLoggerF T.Text
                 A.Object
                 (Logger -> x)
  | HandleRecordF LogRecord
                  x
  | LoggingTimeF (SystemTime -> x)
  deriving (Functor)

-- | Interpret the bunyan DSL into another MonadBunyan (Hint: the ReaderT IO one)
interpretBunyanIO :: MonadBunyan r m => BunyanF a -> m a
interpretBunyanIO (ChildLoggerF n ctx f) = f <$> childLogger n ctx
interpretBunyanIO (HandleRecordF obj x) = handleRecord obj $> x
interpretBunyanIO (LoggingTimeF f) = f <$> getLoggingTime
