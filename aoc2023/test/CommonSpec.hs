module CommonSpec where

import Common (log', logMe', nww)
import Test.Hspec
import Test.QuickCheck

spec = do
  fdescribe "nww" $ do
    it "should be commutative" $ do
      property $ \(Positive a) (Positive b) -> nww a b `shouldBe` nww b a
    it "should be associative" $ do
      property $ \(Positive a) (Positive b) (Positive c) -> nww a (nww b c) `shouldBe` nww (nww a b) c
    it "should be identity" $ do
      property $ \(Positive a) -> nww a a `shouldBe` a
    it "same with identity" $ do
      property $ \(Positive a) -> nww a 1 `shouldBe` a
    it "works" $ do
      property $ \(Positive a) (Positive b) -> nww a b `shouldBe` nww' b a (min b a)
  where
    nww' b a c =
      if c == 1
        then (max a b)
        else if c `mod` a == 0 && c `mod` b == 0 then c else nww' b a (c + 1)