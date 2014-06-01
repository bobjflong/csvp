
module Main where

import Control.Monad.State
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

import Text.ParserCombinators.Parsec hiding (State)

data Command = Grouper Int |
               CommandList [Command] |
               Noop |
               Summer Int |
               StdDever Int |
               Maxer Int |
               Minner Int |
               Averager Int deriving (Show)

source = "(stdin)"

command2Function :: Command -> (GroupedCSV -> GroupedCSV)
command2Function (Grouper x)      = regroupGroupedCSV x
command2Function (Averager x)     = summarizeCSV avgBy x
command2Function (StdDever x)     = summarizeCSV stddevBy x
command2Function (Maxer x)        = summarizeCSV maxBy x
command2Function (Minner x)       = summarizeCSV minBy x
command2Function (Summer x)       = summarizeCSV sumBy x
command2Function (Noop)           = id
command2Function (CommandList xs) = foldl (.) id (map command2Function $ reverse xs)

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

commandSummarizePossibilities = (try $ parseTransformer Summer "sum") <|>
                                (try $ parseTransformer Minner "min") <|>
                                (try $ parseTransformer Maxer "max") <|>
                                (try $ parseTransformer Averager "avg") <|>
                                (try $ parseTransformer StdDever "stddev")

commandGroupPossibilities = try $ parseTransformer Grouper "group"

parseCommands :: GenParser Char st Command
parseCommands =
  do groupResult <- many $ commandGroupPossibilities
     summarizeResult <- many $ commandSummarizePossibilities
     return $ CommandList $ groupResult ++ summarizeResult

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
possibleNumberCSVToString x = unlines $ map (intercalate ",") rows
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

extractColumn :: Int -> PossibleNumberCSV -> [Double]
extractColumn i lst = lst^..traverse.ix i^..traverse.numCSV

------------------------------------
-- Summarizer
-- 

minBy :: Summarizer
minBy i lst = minimum $ extractColumn i lst

maxBy :: Summarizer
maxBy i lst = maximum $ extractColumn i lst

sumBy :: Summarizer
sumBy i lst = sum $ extractColumn i lst

avgBy :: Summarizer
avgBy i lst = flip (/) len $ summed
  where len = (fromIntegral $ length column) :: Double
        summed = sum column
        column = extractColumn i lst

stddevBy :: Summarizer
stddevBy i lst = sqrt $ (/) summed len
  where avg = avgBy i lst
        summed = sum fromAvg
        len = (fromIntegral $ length column) :: Double
        fromAvg = map ((**2).(flip (-) avg)) column
        column = extractColumn i lst

csvToPossibleNumbers :: CSV -> PossibleNumberCSV
csvToPossibleNumbers csv = mapped %~ mapped %~ string2PossibleNumber $ csv

------------------------------------
-- Summarizing CSV values
--

type SummarizeResult = [Maybe Double]

summarizeDefault :: SummarizeResult
summarizeDefault = []

summarize :: Summarizer -> Int -> GroupedCSVRow -> GroupedCSVRow
summarize f d (CSVGroup xs) = CSVGroup $ map (summarize f d) xs
summarize f d (CSVContent xs csv_to_summarize) = CSVContent (xs ++ [ (Just $ f d csv_to_summarize) ]) csv_to_summarize 

summarizeCSV :: Summarizer -> Int -> GroupedCSV -> GroupedCSV
summarizeCSV f d = map (summarize f d)

------------------------------------
--  Grouping CSVs
--

data GroupedCSVRow = CSVContent SummarizeResult PossibleNumberCSV | CSVGroup GroupedCSV
type GroupedCSV    = [ GroupedCSVRow ]

instance Show GroupedCSVRow where
  show (CSVContent r p) = possibleNumberCSVToString p ++ (summary2string r)
    where summary2string [] = ""
          summary2string xs = "= " ++ (intercalate " " $ map (show.fromJust) xs) ++ "\n"
  show (CSVGroup   g) = intercalate "\n" (map show g)

rowEquivalence :: (a -> a -> t) -> Int -> [a] -> [a] -> t
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
-- Grouping and summarizing multiple
--

type CSVTransformState = State GroupedCSV GroupedCSV

transformCSV :: [Either ParseError Command] -> CSVTransformState
transformCSV [] =
  do result <- get
     return result
transformCSV (x:xs) =
  do case x of
       Left _ -> error "Invalid command list"
       Right cmd ->
         do result <- get
            let command = command2Function cmd
            put $ command result
            transformCSV xs

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
        let res = csvToGroupedCSV $ csvToPossibleNumbers parsedClean
        putStrLn $ groupedCSVToString $ evalState (transformCSV commands) res
        return ()
