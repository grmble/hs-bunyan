module LogTextSpec where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Test.Hspec
import System.Log.Bunyan.LogText

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "testing toText" $ do
    it "Text" $ do
      toText @T.Text "J\xfdrgen" `shouldBe` "J\xfdrgen"
    it "Lazy Text" $ do
      toText @LT.Text "J\xfdrgen" `shouldBe` "J\xfdrgen"
    it "Bytestring (UTF8)" $ do
      toText @B.ByteString "J\xc3\xbdrgen" `shouldBe` "J\xfdrgen"
    it "Lazy Bytestring (UTF8)" $ do
      toText @LB.ByteString "J\xc3\xbdrgen" `shouldBe` "J\xfdrgen"
    it "Bytestring (Latin1)" $ do
      toText @B.ByteString "J\xfdrgen" `shouldBe` "J\xfdrgen"
    it "Lazy Bytestring (Latin1)" $ do
      toText @LB.ByteString "J\xfdrgen" `shouldBe` "J\xfdrgen"

  describe "testing toBuilder/combine" $ do
    it "Text/Lazy Text" $ do
      toText (("x" :: T.Text) <>: ("y" :: LT.Text)) `shouldBe` "xy"
    it "ByteString/Lazy ByteString" $ do
      toText (("x" :: B.ByteString) <>: ("y" :: LB.ByteString)) `shouldBe` "xy"
