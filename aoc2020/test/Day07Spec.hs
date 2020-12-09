module Day07Spec where

import Common
import Data.Proxy
import Day07 hiding (main)
import Parseable
import Test.Hspec
import Text.Megaparsec as Mega
import Text.RawString.QQ (r)

example1 =
  Mega.parse
    (Common.parse solution)
    "a"
    [r|light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.|]

example2 =
  Mega.parse
    (Common.parse solution)
    "a"
    [r|shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.|]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      fmap (part1 solution) example1 `shouldBe` Right 4
      Mega.parse (parser (Proxy :: Proxy InputLine')) "a" "light red bags contain 1 bright white bag, 2 muted yellow bags."
        `shouldBe` ( Right $
                       (("light", "red"), [(1, ("bright", "white")), (2, ("muted", "yellow"))])
                   )
      Mega.parse (parser (Proxy :: Proxy InputLine')) "a" "dotted black bags contain no other bags."
        `shouldBe` ( Right $
                       (("dotted", "black"), [])
                   )
      fmap (part2 solution) example1 `shouldBe` Right 32
      fmap (part2 solution) example2 `shouldBe` Right 126
