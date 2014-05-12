
module Main where

import Control.Lens
import System.Environment
import Text.CSV
import Debug.Trace
import System.IO
import Data.Maybe

------------------------------------
-- Parsing the DSL for manipulating CSVs
-- eg. 'groupby 2; groupby 1; avg 3;'
--

import Text.ParserCombinators.Parsec

data Command = Grouper Int |
               CommandList [Command] |
               Averager Int deriving (Show)

source = "(stdin)"

separator  = many $ char ' '
userIndex  = many $ digit
terminator = char ';'

parseTransformer f s =
  do separator
     string s
     separator
     groupIx <- userIndex
     separator
     terminator
     return $ f $ read groupIx

commandSummarizePossibilities = try $ parseTransformer Averager "avg"

commandGroupPossibilities = try $ parseTransformer Grouper "group"

parseCommands :: GenParser Char st Command
parseCommands =
  do groupResult <- many $ commandGroupPossibilities
     summarizeResult <- many $ commandSummarizePossibilities
     return $ CommandList $ groupResult ++ summarizeResult

parseCommandsFromArgs :: [String] -> [Either ParseError Command]
parseCommandsFromArgs args = map performParse args
  where performParse arg = parse parseCommands source (arg ++ ";")

------------------------------------
-- Parsing the CSV and manipulating it
--

parseStdinCSV :: String -> Either ParseError CSV
parseStdinCSV csv = parseCSV source csv

type PossibleNumber = Either String Double
type PossibleNumberCSV = [[PossibleNumber]]

numberFromMaybe (Just x) = x
numberFromMaybe _        = error "Error: could not convert to number"

string2PossibleNumber :: String -> PossibleNumber
string2PossibleNumber str = case (reads str) :: [(Double, String)] of
  [(a, "")] -> Right a
  _         -> Left str

-- 2 dimensional average over CSV
-- Throws an error if the user tries to summarize by a string
avgBy :: Int -> PossibleNumberCSV -> Double
avgBy i lst = flip (/) len $ summed
  where len = (fromIntegral $ length converted) :: Double
        summed = sum converted
        converted = lst^..traverse.ix i^..traverse.to (preview _Right).to numberFromMaybe

csvToPossibleNumbers :: CSV -> PossibleNumberCSV
csvToPossibleNumbers csv = mapped %~ mapped %~ string2PossibleNumber $ csv

main =
  do csv <- getContents
     case parseStdinCSV csv of
      Left err -> do hPutStr stderr $ show err
      Right parsed -> do
       -- let converted = mapped %~ (map string2PossibleNumber) $ csv 
        commands <- getArgs
      -- putStrLn $ show $ parseCommandsFromArgs commands
        putStrLn $ show $ avgBy 2 (csvToPossibleNumbers parsed)


