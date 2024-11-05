module Parser where

import Common (Parser)
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char as MegaC
import qualified Text.Megaparsec.Char.Lexer as Mega

lexeme :: Parser a -> Parser a
lexeme = Mega.lexeme MegaC.space

chunk :: String -> Parser String
chunk = lexeme . Mega.chunk

int :: Read a => Parser a
int = lexeme $ do
  mm <- Mega.try $ Mega.optional "-"
  digits <- Mega.some Mega.digitChar
  pure (read $ (<> digits) $ fromMaybe "" mm)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy del p = lexeme $ do
  Mega.sepBy p del

choice :: [Parser a] -> Parser a
choice = lexeme . Mega.choice
