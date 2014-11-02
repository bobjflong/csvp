{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings #-}

module Summarizer (
    minBy
  , maxBy
  , sumBy
  , avgBy
  , stddevBy
  , countBy
  , GroupedCSVRow (..)
  , summarizeCSV
  , regroup
  , GroupedCSV
  , csvToGroupedCSV
  ) where

import           Control.Lens   hiding ((.=))
import           Data.Aeson
import           Data.List      (groupBy, intercalate, sortBy)
import           Data.Maybe
import qualified Data.Text      as T
import           PossibleNumber

data GroupedCSVRow = CSVContent SummarizeResult PossibleNumberCSV | CSVGroup GroupedCSV

numCSV :: Simple Prism PossibleNumber (Maybe Double)
numCSV = prism (\(Just x) -> (Right x) :: PossibleNumber) $ \ i ->
  case i of
    Left _ -> Right Nothing
    Right x -> Right $ Just x

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

type SummarizeResult = [Maybe Double]

summarizeDefault :: SummarizeResult
summarizeDefault = []

summarize :: Summarizer -> Int -> GroupedCSVRow -> GroupedCSVRow
summarize f d (CSVGroup xs) = CSVGroup $ GroupedCSV $ map (summarize f d) (rows xs)
summarize f d (CSVContent xs csv_to_summarize) = CSVContent (xs ++ [ f d csv_to_summarize ]) csv_to_summarize

summarizeCSV :: Summarizer -> Int -> GroupedCSV -> GroupedCSV
summarizeCSV f d csv = GroupedCSV $ map (summarize f d) (rows csv)

newtype GroupedCSV = GroupedCSV { rows :: [ GroupedCSVRow ] }

instance ToJSON GroupedCSV where
  toJSON gcsv = toJSON (map toJSON (rows gcsv))

blankOrShowJust :: (Show a) => (Maybe a) -> String
blankOrShowJust = maybe "" show

instance ToJSON GroupedCSVRow where
  toJSON (CSVGroup gcsv) = toJSON gcsv
  toJSON (CSVContent r p) = object [
    "rows" .= rows,
    "summary" .= (map (blankOrShowJust) r)]
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


