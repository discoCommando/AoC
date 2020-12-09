{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parseable where

import Common (Parser, parseStringAs, unsafeParse)
import Control.Monad (join)
import Data.Kind (Constraint)
import Data.Typeable
import GHC.TypeLits
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

class Parseable from where
  type Ret from :: *
  parser :: Proxy from -> Parser (Ret from)

data Digit

instance Parseable Digit where
  type Ret Digit = Char
  parser _ = Mega.digitChar

data Sized (size :: Nat) d

instance (KnownNat size, Parseable d) => Parseable (Sized size d) where
  type Ret (Sized size d) = [Ret d]
  parser _ = Mega.count (fromIntegral $ natVal (Proxy :: Proxy size)) (parser (Proxy :: Proxy d))

class KnownValue x where
  type Val x :: *
  valVal :: Proxy x -> Val x

data EmptyArr (a :: *) = EmptyArr

instance (Parseable a) => KnownValue (EmptyArr a) where
  type Val (EmptyArr a) = [Ret a]
  valVal _ = []

instance KnownValue () where
  type Val () = ()
  valVal _ = ()

instance (KnownSymbol x) => KnownValue x where
  type Val x = String
  valVal _ = symbolVal (Proxy :: Proxy x)

data Const (v :: Symbol) x

type Chunk v = Const v ()

type Chunk' v = Const v v

instance (KnownSymbol v, KnownValue x) => Parseable (Const v x) where
  type Ret (Const v x) = Val x
  parser _ = parseStringAs (symbolVal (Proxy :: Proxy v)) (valVal (Proxy :: Proxy x))

data a :+: b

data a $> b

data a <$ b

instance (Parseable a, Parseable b) => Parseable (a :+: b) where
  type Ret (a :+: b) = (Ret a, Ret b)
  parser _ = do
    x <- parser (Proxy :: Proxy a)
    y <- parser (Proxy :: Proxy b)
    pure (x, y)

instance (Parseable a, Parseable b) => Parseable (a $> b) where
  type Ret (a $> b) = Ret b
  parser _ = do
    _x :: x <- parser (Proxy :: Proxy a)
    parser (Proxy :: Proxy b)

instance (Parseable a, Parseable b) => Parseable (a <$ b) where
  type Ret (a <$ b) = Ret a
  parser _ = do
    x <- parser (Proxy :: Proxy a)
    _y :: y <- parser (Proxy :: Proxy b)
    pure x

data Choice vs

instance (Parseable x) => Parseable (Choice '[x]) where
  type Ret (Choice '[x]) = Ret x
  parser _ = parser (Proxy :: Proxy x)

instance (Parseable x, Parseable (Choice (x' : xs)), Ret x ~ Ret (Choice (x' : xs))) => Parseable (Choice (x ': x' ': xs)) where
  type Ret (Choice (x ': x' ': xs)) = Ret x
  parser _ =
    Mega.choice
      [ Mega.try $
          parser
            (Proxy :: Proxy x),
        parser
          (Proxy :: Proxy (Choice (x' ': xs)))
      ]

data a <||> b

instance (Parseable a, Parseable b) => Parseable (a <||> b) where
  type Ret (a <||> b) = Either (Ret a) (Ret b)
  parser _ =
    Mega.choice
      [ Left <$> parser (Proxy :: Proxy a),
        Right <$> parser (Proxy :: Proxy b)
      ]

data ToInt v

class HasOnlyDecimal x

instance HasOnlyDecimal Digit

instance HasOnlyDecimal (Many Digit)

instance HasOnlyDecimal (Some Digit)

instance (HasOnlyDecimal a) => HasOnlyDecimal (a <$ b)

instance (HasOnlyDecimal a) => HasOnlyDecimal (b $> a)

instance (HasOnlyDecimal a) => HasOnlyDecimal (Sized s a)

instance (HasOnlyDecimal a, Parseable a, Ret a ~ [Char]) => Parseable (ToInt a) where
  type Ret (ToInt a) = Int
  parser _ = do
    cs <- parser (Proxy :: Proxy a)
    let x = unsafeParse Mega.decimal cs
    pure x

data Many v

instance (Parseable x) => Parseable (Many x) where
  type Ret (Many x) = [Ret x]
  parser _ = Mega.many (parser (Proxy :: Proxy x))

data Some v

instance (Parseable x) => Parseable (Some x) where
  type Ret (Some x) = [Ret x]
  parser _ = Mega.some (parser (Proxy :: Proxy x))

data SepEndBy v sep

instance (Parseable x, Parseable sep) => Parseable (SepEndBy x sep) where
  type Ret (SepEndBy x sep) = [Ret x]
  parser _ = Mega.sepEndBy (parser (Proxy :: Proxy x)) (parser (Proxy :: Proxy sep))

data LEQThan (v :: Nat) d

instance (KnownNat v, Parseable x, Ret x ~ Int) => Parseable (LEQThan v x) where
  type Ret (LEQThan v x) = Ret x
  parser _ = do
    i <- parser (Proxy :: Proxy x)
    let max' = fromIntegral $ natVal (Proxy :: Proxy v)
    if i <= max'
      then pure i
      else fail ("value " ++ show i ++ " is bigger than " ++ show max')

data BEQThan (v :: Nat) d

instance (KnownNat v, Parseable x, Ret x ~ Int) => Parseable (BEQThan v x) where
  type Ret (BEQThan v x) = Ret x
  parser _ = do
    i <- parser (Proxy :: Proxy x)
    let min' = fromIntegral $ natVal (Proxy :: Proxy v)
    if i >= min'
      then pure i
      else fail ("value " ++ show i ++ " is less than " ++ show min')

type Ranged x y d = BEQThan x (LEQThan y d)

data Newline

instance Parseable Newline where
  type Ret Newline = Char
  parser _ = Mega.newline

data Space

instance Parseable Space where
  type Ret Space = Char
  parser _ = Mega.spaceChar

type Spaces = Many Space

type Whitespace = Choice [Space, Newline]

data Join x

type family Unpack a

type instance Unpack [[a]] = [a]

instance (Parseable x, Ret x ~ [[y]]) => Parseable (Join x) where
  type Ret (Join x) = Unpack (Ret x)
  parser _ = do
    yss <- parser (Proxy :: Proxy x)
    pure $ join yss

data NotRequired x

instance (Parseable x) => Parseable (NotRequired x) where
  type Ret (NotRequired x) = Maybe (Ret x)
  parser _ =
    Mega.choice
      [ Just <$> parser (Proxy :: Proxy x),
        pure Nothing
      ]

data NotWhitespace

instance Parseable NotWhitespace where
  type Ret NotWhitespace = Char
  parser _ = Mega.noneOf [' ', '\n']
