{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PossibleNumber (
    csvToPossibleNumbers
  , PossibleNumber(..)
  , PossibleNumberCSV
  , parseStdinCSV
  ) where

import           Control.Lens hiding ((.=))
import           Data.String
import qualified Data.Text    as T
import           Text.CSV

parseStdinCSV csv = parseCSV "(stdin)" csv

data PossibleNumber = TextData T.Text | DoubleData Double deriving (Eq, Ord)

instance Show PossibleNumber where
  show (TextData t) = T.unpack t
  show (DoubleData d) = show d

type PossibleNumberCSV = [[PossibleNumber]]

csvToPossibleNumbers :: CSV -> PossibleNumberCSV
csvToPossibleNumbers csv = mapped %~ mapped %~ fromString $ csv

instance IsString PossibleNumber where
  fromString str = case (reads str) :: [(Double, String)] of
    [(a, "")] -> DoubleData a
    _         -> TextData $ T.pack str
