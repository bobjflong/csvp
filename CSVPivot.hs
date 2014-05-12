
module Main where

import Control.Lens
import System.Environment
import Text.CSV
import Debug.Trace

------------------------------------
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

commandPossibilies = (try $ parseTransformer Averager "avg") <|> (try $ parseTransformer Grouper "group")

parseCommands :: GenParser Char st Command
parseCommands =
  do result <- many $ commandPossibilies
     return $ CommandList result

parseCommandsFromArgs :: [String] -> [Maybe Command]
parseCommandsFromArgs args = map performParse args
  where performParse arg = preview _Right $ parse parseCommands source arg

------------------------------------

-- 2 dimensional average over csv 
avgBy :: Int -> [[Double]] -> Double
avgBy i lst = flip (/) len $ foldl (+) 0.0 $ lst^..traverse.ix i
  where len = (fromIntegral $ length lst) :: Double

process :: String -> String
process x = x

main =
  do commands <- getArgs
     putStrLn $ show $ parseCommandsFromArgs commands
     interact process
