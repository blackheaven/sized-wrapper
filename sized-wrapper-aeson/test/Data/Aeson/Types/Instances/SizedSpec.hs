module Data.Aeson.Types.Instances.SizedSpec
  ( main,
    spec,
  )
where

import Data.Aeson
import Data.Aeson.Types.Instances.Sized ()
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Sized
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "decode" $ do
    describe "regular element" $ do
      it "valid case should parse" $
        decode "[42]" `shouldBe` Just (singleton (Proxy @[Int]) 42)
      it "empty case should fail" $
        decode "[]" `shouldBe` Nothing @(Sized (Exactly 4) String)
    describe "key" $ do
      it "valid case should parse" $
        decode "{\"a\":42}" `shouldBe` Just (M.singleton (singleton (Proxy @String) 'a') (42 :: Int))
      it "empty case should fail" $
        decode "{\"\":42}" `shouldBe` Nothing @(M.Map (Sized (Exactly 4) String) Int)

  describe "encode" $ do
    it "list of Int" $
      encode (singleton (Proxy @[Int]) 42) `shouldBe` "[42]"
    it "keymap" $
      encode (M.singleton (singleton (Proxy @String) 'a') (42 :: Int)) `shouldBe` "{\"a\":42}"
