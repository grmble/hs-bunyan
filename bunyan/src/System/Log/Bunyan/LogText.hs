module System.Log.Bunyan.LogText where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Control.Exception (SomeException, catch)
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid ((<>))

{- | Converting to Text for logging

Bytestrings will be converted via UTF8, unless that
leads to an error, then latin1 is used
-}
class LogText a where
  -- | Convert to Text
  toText :: a -> T.Text
  -- | Convert to a builder
  toBuilder :: a -> TB.Builder


-- | Combine 2 LogTexts
(<>:) :: (LogText a, LogText b) => a -> b -> TB.Builder
(<>:) a b = toBuilder a <> toBuilder b
infixr 6 <>:

instance LogText T.Text where
  toText = id
  toBuilder = TB.fromText

instance LogText LT.Text where
  toText = LT.toStrict
  toBuilder = TB.fromLazyText

instance LogText TB.Builder where
  toText = LT.toStrict . TB.toLazyText
  toBuilder = id

instance LogText String where
  toText = T.pack
  toBuilder = TB.fromString

instance LogText B.ByteString where
  toText bs = unsafePerformIO $ catch tryUtf8 fallback
    where
      tryUtf8 :: IO T.Text
      tryUtf8 = do
        let !x = T.decodeUtf8 bs
        pure x
      fallback :: SomeException -> IO T.Text
      fallback _ = pure $ T.decodeLatin1 bs
  toBuilder = TB.fromText . toText

-- lazy version uses the strict one
-- no lazy exceptions from decoding lazy bytestring
instance LogText LB.ByteString where
  toText bs = toText $ LB.toStrict bs
  toBuilder bs = toBuilder $ LB.toStrict bs
