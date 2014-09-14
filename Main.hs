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

------------------------------------
-- CSVs are parsed and turned into a [[PossibleNumber]]
-- PossibleNumbers encapsulate the idea that a cell can be textual or numeric data

parseStdinCSV csv = parseCSV "(stdin)" csv

type PossibleNumber = Either T.Text Double
type PossibleNumberCSV = [[PossibleNumber]]

csvToPossibleNumbers :: CSV -> PossibleNumberCSV
csvToPossibleNumbers csv = mapped %~ mapped %~ fromString $ csv

------------------------------------
-- Allow us to go to and fro between PossibleNumbers and Doubles
--
numCSV :: Simple Prism PossibleNumber (Maybe Double)
numCSV = prism (\(Just x) -> (Right x) :: PossibleNumber) $ \ i ->
  case i of
    Left _ -> Right Nothing
    Right x -> Right $ Just x

instance IsString PossibleNumber where
  fromString str = case (reads str) :: [(Double, String)] of
    [(a, "")] -> Right a
    _         -> Left $ T.pack str

type Summarizer = Int -> PossibleNumberCSV -> Maybe Double

extractColumn :: Int -> PossibleNumberCSV -> [Double]
extractColumn i lst = catMaybes $ lst^..traverse.ix i.numCSV

------------------------------------
-- Currently implemented summarizer functions
--

nothingIfEmpty :: ([t] -> a) -> [t] -> Maybe a
nothingIfEmpty _ [] = Nothing
nothingIfEmpty f x = Just $ f x

minBy :: Summarizer
minBy i lst = nothingIfEmpty minimum $ extractColumn i lst

maxBy :: Summarizer
maxBy i lst = nothingIfEmpty maximum $ extractColumn i lst

sumBy :: Summarizer
sumBy i lst = nothingIfEmpty sum $ extractColumn i lst

avgBy :: Summarizer
avgBy i lst = Just $ flip (/) len summed
  where len = (fromIntegral $ length column) :: Double
        summed = sum column
        column = extractColumn i lst

stddevBy :: Summarizer
stddevBy i lst = Just $ sqrt $ (/) summed len
  where avg = avgBy i lst
        summed = sum fromAvg
        len = (fromIntegral $ length column) :: Double
        fromAvg = map ((**2).(flip (-) (fromJust avg))) column
        column = extractColumn i lst

countBy :: Summarizer
countBy i lst = Just len
  where len = (fromIntegral $ length column) :: Double
        column = extractColumn i lst

------------------------------------
-- Mapping summarizers over data, accumulating results
--

type SummarizeResult = [Maybe Double]

summarizeDefault :: SummarizeResult
summarizeDefault = []

summarize :: Summarizer -> Int -> GroupedCSVRow -> GroupedCSVRow
summarize f d (CSVGroup xs) = CSVGroup $ GroupedCSV $ map (summarize f d) (rows xs)
summarize f d (CSVContent xs csv_to_summarize) = CSVContent (xs ++ [ f d csv_to_summarize ]) csv_to_summarize

summarizeCSV :: Summarizer -> Int -> GroupedCSV -> GroupedCSV
summarizeCSV f d csv = GroupedCSV $ map (summarize f d) (rows csv)

------------------------------------
--  CSVs can be grouped, then subgrouped
--  GoupedCSVRows are either content with summarizer results | or a pointer to another subgroup

data GroupedCSVRow = CSVContent SummarizeResult PossibleNumberCSV | CSVGroup GroupedCSV

newtype GroupedCSV = GroupedCSV { rows :: [ GroupedCSVRow ] }

instance ToJSON GroupedCSV where
  toJSON gcsv = toJSON (map toJSON (rows gcsv))

blankOrShowJust :: (Show a) => (Maybe a) -> String
blankOrShowJust = maybe "" show

instance ToJSON GroupedCSVRow where
  toJSON (CSVGroup gcsv) = toJSON gcsv
  toJSON (CSVContent r p) = object [
    (T.pack "rows") .= rows,
    (T.pack "summary") .= (map (blankOrShowJust) r)]
    where rows = map (map (either T.unpack show)) p

instance Show GroupedCSV where
  show = (intercalate "").(map show).rows

instance Show GroupedCSVRow where
  show (CSVContent r p) = showPossibleNumberCSV ++ (summary2string r)
    where summary2string [] = ""
          summary2string xs = "= " ++ (unwords $ map (blankOrShowJust) xs) ++ "\n"
          showPossibleNumberCSV = unlines $ map (intercalate ",") rows
          rows = map (map (either T.unpack show)) p

  show (CSVGroup   g) = intercalate "\n" (map show (rows g))

class GroupableByIndex a b where
  regroup :: Int -> a -> b

instance GroupableByIndex GroupedCSV GroupedCSV where
  regroup x csv = GroupedCSV $ map (regroup x) (rows csv)

instance GroupableByIndex GroupedCSVRow GroupedCSVRow where
  regroup x (CSVContent _ p) = CSVGroup $ GroupedCSV $ regroup x p
  regroup x (CSVGroup   g) = CSVGroup $ GroupedCSV $ map (regroup x) (rows g)

instance GroupableByIndex PossibleNumberCSV [ GroupedCSVRow ] where
  regroup x p = map (CSVContent summarizeDefault) grouped
    where grouped = groupBy (rowEquivalence (==) x) $ sortBy (rowEquivalence compare x) p
          rowEquivalence :: (a -> a -> t) -> Int -> [a] -> [a] -> t
          rowEquivalence f x l r = f (l!!x) (r!!x)

csvToGroupedCSV :: PossibleNumberCSV -> GroupedCSV
csvToGroupedCSV x = GroupedCSV $ [ CSVContent summarizeDefault x ]

transformCSV :: Either ParseError Command -> GroupedCSV -> Either ErrorString GroupedCSV
transformCSV x res =
  do case x of
       Left _ -> Left "Invalid command list"
       Right cmd -> let command = toCSVProcessor cmd in Right $ command res

main =
  do csv <- getContents
     case parseStdinCSV csv of
      Left err -> hPutStr stderr $ show err
      Right parsed -> do
        arguments <- getArgs
        case arguments of
          (i:o) -> do let commands = parseCommandsFromArgs i
                      let parsedClean = delete [""] parsed
                      let res = csvToGroupedCSV $ csvToPossibleNumbers parsedClean
                      case transformCSV commands res of
                        Right t -> case o of
                                               [] -> do putStrLn $ show $ t
                                               ["--json"] -> do putStrLn $ BSL.unpack.encode $ toJSON $ t
                                               x -> do hPutStr stderr $ "Unknown output type: " ++ (concat x)
                        Left err -> hPutStr stderr err
                      return ()
          _ -> hPutStr stderr "Please specify some commands, eg 'group 0; avg 2;'"

