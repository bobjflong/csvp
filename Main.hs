{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -O2 #-}

module Main where

import Control.Lens hiding ((.=))
import Data.String
import System.Environment
import Text.CSV
import System.IO
import Data.Monoid
import Data.Aeson
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import qualified Data.Text as T
import Data.List (groupBy, sortBy, delete, intercalate)
import qualified Data.ByteString.Lazy.Char8 as BSL
import PossibleNumber
import Summarizer
import System.Console.ArgParser

import Text.ParserCombinators.Parsec hiding (State)

type ErrorString = String

data Command = Grouper Int
               | CommandList [Command]
               | Noop
               | Summer Int
               | StdDever Int
               | Maxer Int
               | Minner Int
               | Counter Int
               | Averager Int deriving (Show, Eq)

instance Monoid Command where
  mempty = Noop
  mappend x Noop = x
  mappend Noop x = x
  mappend (CommandList x) (CommandList y) = CommandList $ x ++ y
  mappend (CommandList x) y = CommandList $ x ++ [y]
  mappend y (CommandList x) = CommandList $ [y] ++ x
  mappend x y = CommandList [x, y]

separator  = many $ char ' '
userIndex  = many $ digit
terminator = char ';'

parseTransformer f s =
  do _ <- separator
     _ <- string s
     _ <- separator
     groupIx <- userIndex
     _ <- separator
     _ <- terminator
     return $ f $ read groupIx

commandSummarizePossibilities :: GenParser Char st Command
commandSummarizePossibilities = (try $ parseTransformer Summer "sum") <|>
                                (try $ parseTransformer Minner "min") <|>
                                (try $ parseTransformer Maxer "max") <|>
                                (try $ parseTransformer Averager "avg") <|>
                                (try $ parseTransformer Counter "count") <|>
                                (try $ parseTransformer StdDever "stddev")

commandGroupPossibilities :: GenParser Char st Command
commandGroupPossibilities = try $ parseTransformer Grouper "group"

parseCommands :: GenParser Char st Command
parseCommands =
  do groupResult <- many commandGroupPossibilities
     summarizeResult <- many commandSummarizePossibilities
     return $ mconcat $ groupResult ++ summarizeResult

parseCommandsFromArgs :: String -> Either ParseError Command
parseCommandsFromArgs = performParse
  where performParse arg = parse parseCommands "(stdin)" (arg ++ ";")

toCSVProcessor :: Command -> GroupedCSV -> GroupedCSV
toCSVProcessor (Grouper x)      = regroup x
toCSVProcessor (Averager x)     = summarizeCSV avgBy x
toCSVProcessor (StdDever x)     = summarizeCSV stddevBy x
toCSVProcessor (Maxer x)        = summarizeCSV maxBy x
toCSVProcessor (Minner x)       = summarizeCSV minBy x
toCSVProcessor (Summer x)       = summarizeCSV sumBy x
toCSVProcessor (Counter x)      = summarizeCSV countBy x
toCSVProcessor (Noop)           = id
toCSVProcessor (CommandList xs) = foldl (.) id (map toCSVProcessor $ reverse xs)

transformCSV :: Either ParseError Command -> GroupedCSV -> Either ErrorString GroupedCSV
transformCSV x res =
  do case x of
       Left _ -> Left "Invalid command list"
       Right cmd -> let command = toCSVProcessor cmd in Right $ command res

data CSVPArguments = CSVPArguments String String deriving (Show)

csvpArgumentsParser :: ParserSpec CSVPArguments
csvpArgumentsParser = CSVPArguments `parsedBy` reqPos "commands" `andBy` optPos "default" "output type"

csvp (CSVPArguments i o) =
  do csv <- getContents
     case parseStdinCSV csv of
      Left err -> hPutStr stderr $ show err
      Right parsed -> do
        let commands = parseCommandsFromArgs i
        let parsedClean = delete [""] parsed
        let res = csvToGroupedCSV $ csvToPossibleNumbers parsedClean
        case transformCSV commands res of
          Right t -> case o of
            "default" -> do putStrLn $ show $ t
            "json" -> do putStrLn $ BSL.unpack.encode $ toJSON $ t
            x -> do hPutStr stderr $ "Unknown output type: " ++ x
          Left err -> hPutStr stderr err
        return ()

main = withParseResult csvpArgumentsParser csvp

