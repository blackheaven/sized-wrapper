module Data.SizedSpec
  ( main,
    spec,
  )
where

import Data.Proxy
import Data.Sized
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Singleton creation should be equivalent to a single element list" $
    singleton (Proxy @[Int]) 42 `shouldBe` trustedSized [42]
  it "nonEmpty creation on filled list should be equivalent to the wrapped list" $
    nonEmpty @[Int] [1, 2, 3] `shouldBe` Just (trustedSized [1, 2, 3])
  it "nonEmpty creation on empty list should be Nothing" $
    nonEmpty @[Int] [] `shouldBe` Nothing
  it "prepend should use semigroup" $
    singleton (Proxy @[Int]) 1 <| [2] `shouldBe` trustedSized [1, 2]
  it "append should use semigroup" $
    [1] |> singleton (Proxy @[Int]) 2 `shouldBe` trustedSized [1, 2]
