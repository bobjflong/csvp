{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PossibleNumber (
    csvToPossibleNumbers
  , PossibleNumber
  , PossibleNumberCSV
  , parseStdinCSV
  ) where

import           Control.Lens hiding ((.=))
import           Data.String
import qualified Data.Text    as T
import           Text.CSV

parseStdinCSV csv = parseCSV "(stdin)" csv

type PossibleNumber = Either T.Text Double
type PossibleNumberCSV = [[PossibleNumber]]

csvToPossibleNumbers :: CSV -> PossibleNumberCSV
csvToPossibleNumbers csv = mapped %~ mapped %~ fromString $ csv

instance IsString PossibleNumber where
  fromString str = case (reads str) :: [(Double, String)] of
    [(a, "")] -> Right a
    _         -> Left $ T.pack str
