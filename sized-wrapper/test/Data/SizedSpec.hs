{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoOverloadedStrings #-}

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
  describe "sized creation" $ do
    describe "Exactly" $ do
      it "(Exactly 3) on three items list should be equivalent to the wrapped list" $
        sized @(Exactly 3) @[Int] [1, 2, 3] `shouldBe` Just (trustedSized [1, 2, 3])
      it "(Exactly 0) on empty list should be equivalent to the wrapped list" $
        sized @(Exactly 0) @[Int] [] `shouldBe` Just (trustedSized [])
      it "(Exactly 3) on empty list should be Nothing" $
        sized @(Exactly 3) @[Int] [] `shouldBe` Nothing
    describe "AtLeast" $ do
      it "(AtLeast 3) on three items list should be equivalent to the wrapped list" $
        sized @(AtLeast 3) @[Int] [1, 2, 3] `shouldBe` Just (trustedSized [1, 2, 3])
      it "(AtLeast 0) on empty list should be equivalent to the wrapped list" $
        sized @(AtLeast 0) @[Int] [] `shouldBe` Just (trustedSized [])
      it "(AtLeast 3) on empty list should be Nothing" $
        sized @(AtLeast 3) @[Int] [] `shouldBe` Nothing
    describe "AtMost" $ do
      it "(AtMost 3) on three items list should be equivalent to the wrapped list" $
        sized @(AtMost 3) @[Int] [1, 2, 3] `shouldBe` Just (trustedSized [1, 2, 3])
      it "(AtMost 0) on empty list should be equivalent to the wrapped list" $
        sized @(AtMost 0) @[Int] [] `shouldBe` Just (trustedSized [])
      it "(AtMost 3) on a 4-items list should be Nothing" $
        sized @(AtMost 3) @[Int] [1, 2, 3, 4] `shouldBe` Nothing
    describe "Between" $ do
      it "(Between 3 3) on three items list should be equivalent to the wrapped list" $
        sized @(Between 3 3) @[Int] [1, 2, 3] `shouldBe` Just (trustedSized [1, 2, 3])
      it "(Between 0 0) on empty list should be equivalent to the wrapped list" $
        sized @(Between 0 0) @[Int] [] `shouldBe` Just (trustedSized [])
      it "(Between 3 3) on a 4-items list should be Nothing" $
        sized @(Between 3 3) @[Int] [1, 2, 3, 4] `shouldBe` Nothing
      it "(Between 2 4) on three items list should be equivalent to the wrapped list" $
        sized @(Between 2 4) @[Int] [1, 2, 3] `shouldBe` Just (trustedSized [1, 2, 3])
      it "(Between 0 4) on empty list should be equivalent to the wrapped list" $
        sized @(Between 0 4) @[Int] [] `shouldBe` Just (trustedSized [])
      it "(Between 1 3) on a 4-items list should be Nothing" $
        sized @(Between 1 3) @[Int] [1, 2, 3, 4] `shouldBe` Nothing
      it "(Between 5 7) on a 4-items list should be Nothing" $
        sized @(Between 5 7) @[Int] [1, 2, 3, 4] `shouldBe` Nothing
  describe "approximate" $ do
    describe "Exactly" $ do
      it "(Exactly 3) approximates to (Between 3 3)" $
        approximate (trustedSized @[Int] @(Exactly 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(Between 3 3) [1, 2, 3]
      it "(Exactly 3) approximates to (Between 0 3)" $
        approximate (trustedSized @[Int] @(Exactly 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(Between 0 3) [1, 2, 3]
      it "(Exactly 3) approximates to (Between 3 5)" $
        approximate (trustedSized @[Int] @(Exactly 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(Between 3 5) [1, 2, 3]
      it "(Exactly 3) approximates to (AtLeast 3)" $
        approximate (trustedSized @[Int] @(Exactly 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(AtLeast 3) [1, 2, 3]
      it "(Exactly 3) approximates to (AtMost 3)" $
        approximate (trustedSized @[Int] @(Exactly 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(AtMost 3) [1, 2, 3]
      it "(Exactly 3) approximates to (AtLeast 1)" $
        approximate (trustedSized @[Int] @(Exactly 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(AtLeast 1) [1, 2, 3]
      it "(Exactly 3) approximates to (AtMost 5)" $
        approximate (trustedSized @[Int] @(Exactly 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(AtMost 5) [1, 2, 3]
    describe "AtLeast" $ do
      it "(AtLeast 3) approximates to (AtLeast 2)" $
        approximate (trustedSized @[Int] @(AtLeast 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(AtLeast 2) [1, 2, 3]
    describe "AtMost" $ do
      it "(AtMost 3) approximates to (AtMost 4)" $
        approximate (trustedSized @[Int] @(AtMost 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(AtMost 4) [1, 2, 3]
    describe "Between" $ do
      it "(Between 3 3) approximates to (Between 3 3)" $
        approximate (trustedSized @[Int] @(Between 3 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(Between 3 3) [1, 2, 3]
      it "(Between 3 3) approximates to (Between 0 3)" $
        approximate (trustedSized @[Int] @(Between 3 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(Between 0 3) [1, 2, 3]
      it "(Between 3 3) approximates to (Between 3 5)" $
        approximate (trustedSized @[Int] @(Between 3 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(Between 3 5) [1, 2, 3]
      it "(Between 3 3) approximates to (AtLeast 3)" $
        approximate (trustedSized @[Int] @(Between 3 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(AtLeast 3) [1, 2, 3]
      it "(Between 3 3) approximates to (AtMost 3)" $
        approximate (trustedSized @[Int] @(Between 3 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(AtMost 3) [1, 2, 3]
      it "(Between 3 3) approximates to (AtLeast 1)" $
        approximate (trustedSized @[Int] @(Between 3 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(AtLeast 1) [1, 2, 3]
      it "(Between 3 3) approximates to (AtMost 5)" $
        approximate (trustedSized @[Int] @(Between 3 3) [1, 2, 3]) `shouldBe` trustedSized @[Int] @(AtMost 5) [1, 2, 3]

-- it "prepend should use semigroup" $
--   singleton (Proxy @[Int]) 1 <| [2] `shouldBe` trustedSized [1, 2]
-- it "append should use semigroup" $
--   [1] |> singleton (Proxy @[Int]) 2 `shouldBe` trustedSized [1, 2]
