{-# LANGUAGE NumericUnderscores #-}

module Day07 where

import Common
import Control.Monad.Identity (runIdentity)
import Data.Data
import Data.List (group, sortOn)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShowId)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data LsInfo = File {size :: Integer, name :: String} | Directory {name :: String}
  deriving stock (Generic, Show, Eq)

lsInfoParser :: Parser LsInfo
lsInfoParser =
  Mega.choice
    [ Mega.try $
        Directory <$> do
          Mega.chunk "dir "
          Mega.some Mega.printChar,
      File
        <$> Mega.decimal
        <*> do
          Mega.hspace
          Mega.some Mega.printChar
    ]

data Command = Ls [LsInfo] | Cd String
  deriving stock (Generic, Show, Eq)

commandParser :: Parser Command
commandParser = do
  Mega.chunk "$ "
  Mega.choice
    [ Mega.try $
        Cd <$> do
          Mega.chunk "cd "
          Mega.some Mega.printChar,
      Ls <$> do
        Mega.chunk "ls"
        Mega.newline
        Mega.sepEndBy lsInfoParser Mega.newline
    ]

type InputLine = Command

solution :: Solution [InputLine] Integer Integer
solution =
  Solution
    { parse = Mega.sepEndBy commandParser (Mega.try Mega.newline <|> pure 'x'), -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

-- /abc/d -> [d, abc]
-- / -> []
type Path' = [String]

type FileStructure = Map.Map Path' [LsInfo]

stringToPath' :: String -> Path'
stringToPath' ('/' : rest) =
  let first = takeWhile (/= '/') rest
      rest' = dropWhile (/= '/') rest
   in stringToPath' rest' ++ [first]
stringToPath' [] = []

buildFileStructure :: [Command] -> Path' -> FileStructure -> FileStructure
buildFileStructure [] _ fs = fs
buildFileStructure (command : rest) currentPath fs =
  case command of
    Ls lsInfo ->
      buildFileStructure rest currentPath $ Map.insert currentPath lsInfo fs
    Cd s ->
      let newPath =
            case s of
              "/" -> []
              ".." -> tail currentPath
              _ -> s : currentPath
       in buildFileStructure rest newPath fs

totalSize :: Path' -> FileStructure -> (Integer, Map.Map Path' Integer)
totalSize currentPath fs =
  let contents = fs Map.! currentPath
      (ts, nm) =
        foldl
          ( \(tsize, m') -> \case
              File {..} -> (tsize + size, m')
              Directory {..} ->
                let (s, m2) = totalSize (name : currentPath) fs in (tsize + s, m' `Map.union` m2)
          )
          (0, Map.empty)
          contents
   in (ts, Map.insert currentPath ts nm)

part1' :: [InputLine] -> Integer
part1' commands = runIdentity $ do
  let fileStructure = buildFileStructure commands [] Map.empty
  let (_, newMap) = totalSize [] fileStructure
  let smallerThan = filter (\x -> snd x <= 100_000) $ Map.toList newMap
  pure $ sum $ snd <$> smallerThan

totalSize' = 70_000_000

minimalSize' = 30_000_000

part2' :: [InputLine] -> Integer
part2' commands = runIdentity $ do
  let fileStructure = buildFileStructure commands [] Map.empty
  let (_, newMap) = totalSize [] fileStructure
  let currentFreeSpace = totalSize' - newMap Map.! []
  let goodEnough = filter (\x -> currentFreeSpace + snd x >= minimalSize') $ Map.toList newMap
  let sorted = sortOn snd goodEnough
  pure $ head $ snd <$> sorted

main =
  aoc
    "07"
    solution
