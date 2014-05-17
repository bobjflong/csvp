
module Main where

import Control.Lens
import Control.Lens.Prism
import System.Environment
import Text.CSV
import Debug.Trace
import System.IO
import Data.Maybe
import Data.List (groupBy, sortBy, delete, intercalate)

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
     summarizeResult <- commandSummarizePossibilities
     return $ CommandList $ groupResult ++ [summarizeResult]

parseCommandsFromArgs :: [String] -> [Either ParseError Command]
parseCommandsFromArgs = map performParse
  where performParse arg = parse parseCommands source (arg ++ ";")

------------------------------------
-- Parsing the CSV and manipulating it
--

parseStdinCSV :: String -> Either ParseError CSV
parseStdinCSV csv = parseCSV source csv

type PossibleNumber = Either String Double
type PossibleNumberCSV = [[PossibleNumber]]

possibleNumberToString :: PossibleNumber -> String
possibleNumberToString (Left x) = x
possibleNumberToString (Right x) = show x

possibleNumberCSVToString :: PossibleNumberCSV -> String
possibleNumberCSVToString x = intercalate "\n" $ map (intercalate ",") rows
  where rows = map (map possibleNumberToString) x

numCSV :: Simple Prism PossibleNumber Double
numCSV = prism (\x -> (Right x) :: PossibleNumber) $ \ i ->
  case i of
    Left _ -> error "Error: could not convert column to number"
    Right x -> Right x

string2PossibleNumber :: String -> PossibleNumber
string2PossibleNumber str = case (reads str) :: [(Double, String)] of
  [(a, "")] -> review numCSV a
  _         -> Left str

type Summarizer = Int -> PossibleNumberCSV -> Double

-- 2 dimensional average over CSV
-- Throws an error if the user tries to summarize by a string
avgBy :: Summarizer
avgBy i lst = flip (/) len $ summed
  where len = (fromIntegral $ length converted) :: Double
        summed = sum converted
        converted = lst^..traverse.ix i^..traverse.numCSV

csvToPossibleNumbers :: CSV -> PossibleNumberCSV
csvToPossibleNumbers csv = mapped %~ mapped %~ string2PossibleNumber $ csv

------------------------------------
--  Grouping CSVs
--

type SummarizeResult = Double

summarizeDefault :: SummarizeResult
summarizeDefault = 0

data GroupedCSVRow = CSVContent SummarizeResult PossibleNumberCSV | CSVGroup GroupedCSV
type GroupedCSV    = [ GroupedCSVRow ]

summarize :: Summarizer -> Int -> GroupedCSVRow -> GroupedCSVRow
summarize f d (CSVGroup xs) = CSVGroup $ map (summarize f d) xs
summarize f d (CSVContent _ csv) = CSVContent (f d csv) csv 

summarizeCSV :: Summarizer -> Int -> GroupedCSV -> GroupedCSV
summarizeCSV f d = map (summarize f d)

instance Show GroupedCSVRow where
  show (CSVContent _ p) = possibleNumberCSVToString p
  show (CSVGroup   g) = intercalate "\n\n" (map show g)

rowEquivalence f x = \l r ->  f (l!!x) (r!!x)

groupByCSVIndex :: Int -> PossibleNumberCSV -> [ GroupedCSVRow ]
groupByCSVIndex x p = map (CSVContent summarizeDefault) $ groupBy (rowEquivalence (==) x) $ sortBy (rowEquivalence compare x) p

csvToGroupedCSV :: PossibleNumberCSV -> GroupedCSV
csvToGroupedCSV x = [ CSVContent summarizeDefault x ]

groupedCSVToString :: GroupedCSV -> String
groupedCSVToString x = intercalate "" $ map show x

regroupGroupedCSVRow :: Int -> GroupedCSVRow -> GroupedCSVRow
regroupGroupedCSVRow x (CSVContent _ p) = CSVGroup $ groupByCSVIndex x p 
regroupGroupedCSVRow x (CSVGroup   g) = CSVGroup $ map (regroupGroupedCSVRow x) g

regroupGroupedCSV :: Int -> GroupedCSV -> GroupedCSV
regroupGroupedCSV x p = map (regroupGroupedCSVRow x) p

------------------------------------
-- Main
--

main =
  do csv <- getContents
     case parseStdinCSV csv of
      Left err -> do hPutStr stderr $ show err
      Right parsed -> do
        instructions <- getArgs
        let commands = parseCommandsFromArgs instructions
        let parsedClean = delete [""] parsed
        let res = regroupGroupedCSV 0 $ regroupGroupedCSV 1 $ csvToGroupedCSV $ csvToPossibleNumbers parsedClean
        putStrLn "-----------------------"
        putStrLn $ groupedCSVToString res
        return ()

