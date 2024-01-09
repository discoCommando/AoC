module IntervalSpec where

import Interval
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (subtract)

instance Arbitrary SingleInterval where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary `suchThat` (>= start)
    pure $ mkSingleInterval start end

instance Arbitrary Interval where
  arbitrary = do
    intervals <- listOf arbitrary
    pure $ fromList' intervals

spec =
  describe "operations" $ do
    describe "union" $ do
      it "should be commutative" $
        property $
          \i1 i2 -> union i1 i2 `shouldBe` union i2 i1
      it "should be associative" $
        property $
          \i1 i2 i3 -> union i1 (union i2 i3) `shouldBe` union (union i1 i2) i3
      it "should be idempotent" $
        property $
          \i -> union i i `shouldBe` i
      it "should be identity" $
        property $
          \i -> union i mempty `shouldBe` i
    describe "intersect" $ do
      it "should be commutative" $
        property $
          \i1 i2 -> intersect i1 i2 `shouldBe` intersect i2 i1
      it "should be associative" $
        property $
          \i1 i2 i3 -> intersect i1 (intersect i2 i3) `shouldBe` intersect (intersect i1 i2) i3
      it "should be idempotent" $
        property $
          \i -> intersect i i `shouldBe` i
      it "should be identity" $
        property $
          \i -> intersect i mempty `shouldBe` mempty
    describe "subtract" $ do
      it "should be identity" $
        property $
          \i -> subtract i mempty `shouldBe` i
      it "should be idempotent" $
        property $
          \i -> subtract i i `shouldBe` mempty
      it "should be commutative" $
        property $
          \i1 i2 i3 -> subtract (subtract i1 i2) i3 `shouldBe` subtract (subtract i1 i3) i2
      it "with intersect" $
        property $
          \i1 i2 -> subtract i1 i2 `shouldBe` subtract i1 (intersect i1 i2)
      it "with union" $
        property $
          \i1 i2 -> subtract (union i1 i2) i2 `shouldBe` subtract i1 (intersect i1 i2)
