module Day10Spec where

import Common
import Day10 hiding (main)
import Test.Hspec
import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    [r|16
10
15
5
1
11
7
19
6
12
4|]

example2 =
  unsafeParseExample
    solution
    [r|28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3|]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      part1 solution example1 `shouldBe` 35
      part1 solution example2 `shouldBe` 220
      part2 solution example1 `shouldBe` 8
      part2 solution example2 `shouldBe` 19208
