{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Passport where

import Common
import Control.Lens ((&), (.~), (^.), (^?), view)
import Control.Lens.Extras (is)
import Data.Typeable
import Debug.Trace
import GHC.TypeLits
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data FieldName
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportID
  | CountryID
  deriving stock (Generic, Show, Eq, Enum, Bounded)

instance KnownValue BirthYear where
  type Val BirthYear = FieldName
  valVal _ = BirthYear

instance KnownValue IssueYear where
  type Val IssueYear = FieldName
  valVal _ = IssueYear

instance KnownValue ExpirationYear where
  type Val ExpirationYear = FieldName
  valVal _ = ExpirationYear

instance KnownValue Height where
  type Val Height = FieldName
  valVal _ = Height

instance KnownValue HairColor where
  type Val HairColor = FieldName
  valVal _ = HairColor

instance KnownValue EyeColor where
  type Val EyeColor = FieldName
  valVal _ = EyeColor

instance KnownValue PassportID where
  type Val PassportID = FieldName
  valVal _ = PassportID

instance KnownValue CountryID where
  type Val CountryID = FieldName
  valVal _ = CountryID

type family FieldValue (k :: FieldName)

type instance FieldValue BirthYear = Ranged 1920 2002 (ToInt (Sized 4 Digit))

type instance FieldValue IssueYear = Ranged 2010 2020 (ToInt (Sized 4 Digit))

type instance FieldValue ExpirationYear = Ranged 2020 2030 (ToInt (Sized 4 Digit))

type instance
  FieldValue Height =
    Choice
      [ Ranged 59 76 (ToInt (Sized 2 Digit <$ Chunk "in")),
        Ranged 150 193 (ToInt (Sized 3 Digit <$ Chunk "cm"))
      ]

type instance
  FieldValue HairColor =
    Chunk "#"
      $> Join
           ( Sized
               6
               ( Choice
                   [ Sized 1 Digit,
                     Chunk' "a",
                     Chunk' "b",
                     Chunk' "c",
                     Chunk' "d",
                     Chunk' "e",
                     Chunk' "f"
                   ]
               )
           )

type instance
  FieldValue EyeColor =
    Choice
      [ Chunk' "amb",
        Chunk' "blu",
        Chunk' "brn",
        Chunk' "gry",
        Chunk' "grn",
        Chunk' "hzl",
        Chunk' "oth"
      ]

type instance FieldValue PassportID = Sized 9 Digit

type instance FieldValue CountryID = NotRequired (Many NotWhitespace)

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.

-- type instance FieldValue IssueYear = SizedInt 4 2010 2020

-- type instance FieldValue ExpirationYear = SizedInt 4

fieldNameParser :: Parser FieldName
fieldNameParser =
  Mega.choice
    [ parseStringAs "byr:" BirthYear,
      parseStringAs "iyr:" IssueYear,
      parseStringAs "eyr:" ExpirationYear,
      parseStringAs "hgt:" Height,
      parseStringAs "hcl:" HairColor,
      parseStringAs "ecl:" EyeColor,
      parseStringAs "pid:" PassportID,
      parseStringAs "cid:" CountryID
    ]

data ValidPassport = ValidPassport
  { birthYear :: Ret (FieldValue BirthYear),
    issueYear :: Ret (FieldValue IssueYear),
    expirationYear :: Ret (FieldValue ExpirationYear),
    height :: Ret (FieldValue Height),
    hairColor :: Ret (FieldValue HairColor),
    eyeColor :: Ret (FieldValue EyeColor),
    passportID :: Ret (FieldValue PassportID),
    countryID :: Ret (FieldValue CountryID)
  }
  deriving (Generic, Show, Eq)

data Field n v = Field {name :: n, value :: v}
  deriving stock (Generic, Show, Eq)

findByName :: FieldName -> [Field FieldName a] -> Maybe (Field FieldName a)
findByName p fs = case filter ((==) p . view #name) fs of
  [x] -> Just x
  _ -> Nothing

hasAllFieldNames :: [Field FieldName String] -> Bool
hasAllFieldNames fs =
  let f x = is #_Just $ findByName x fs
   in and
        [ f BirthYear,
          f IssueYear,
          f ExpirationYear,
          f Height,
          f HairColor,
          f EyeColor,
          f PassportID
        ]

fieldParser :: Parser a -> Parser (Field FieldName a)
fieldParser pa =
  Field <$> fieldNameParser <*> pa

fieldValueParser :: forall v. (Parseable (FieldValue v)) => Proxy v -> Parser (Ret (FieldValue v))
fieldValueParser _ = do
  val <- parser (Proxy :: Proxy (FieldValue v))
  rest <- Mega.takeWhileP Nothing (\a -> a == ' ' || a == '\n')
  if null rest
    then pure val
    else fail "not all input eaten"

findFullField :: forall v. (KnownValue v, Val v ~ FieldName, Parseable (FieldValue v)) => Proxy v -> [Field FieldName String] -> Maybe (Field FieldName (Ret (FieldValue v)))
findFullField p fields = do
  let name = valVal p
  f <- findByName name fields
  let a = case Mega.parse (fieldValueParser p) "a" (view #value f) of
        Left e -> trace (show e) Nothing
        Right v -> Just v
  v <- a
  pure (f & #value .~ v)

findFieldValue :: forall v. (KnownValue v, Val v ~ FieldName, Parseable (FieldValue v)) => Proxy v -> [Field FieldName String] -> Maybe (Ret (FieldValue v))
findFieldValue p fields = view #value <$> findFullField p fields

tryCreateValidPassport :: [Field FieldName String] -> Maybe ValidPassport
tryCreateValidPassport fields =
  ValidPassport
    <$> findFieldValue (Proxy :: Proxy BirthYear) fields
    <*> findFieldValue (Proxy :: Proxy IssueYear) fields
    <*> findFieldValue (Proxy :: Proxy ExpirationYear) fields
    <*> findFieldValue (Proxy :: Proxy Height) fields
    <*> findFieldValue (Proxy :: Proxy HairColor) fields
    <*> findFieldValue (Proxy :: Proxy EyeColor) fields
    <*> findFieldValue (Proxy :: Proxy PassportID) fields
    <*> (findFieldValue (Proxy :: Proxy CountryID) fields <|> pure Nothing)
