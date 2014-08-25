
module Test where

import qualified Main as M

import Test.HUnit
import Control.Monad.State
import Control.Lens hiding (elements)
import Text.Parsec.Error
import Data.Monoid
import System.IO.Unsafe
import Test.QuickCheck

basicCommandListParsing = TestCase (assertEqual "basic command list" parsed fromStr)
  where fromStr = (^?_Right) $ M.parseCommandsFromArgs "group 0; group 1; avg 2;"
        parsed  = Just $ M.CommandList [M.Grouper 0, M.Grouper 1, M.Averager 2]

multipleSummarizers = TestCase (assertEqual "multiple summarizers" parsed fromStr)
  where fromStr = (^?_Right) $ M.parseCommandsFromArgs $ "group 0; avg 2; stddev 2; max 2;"
        parsed  = Just $ M.CommandList [M.Grouper 0, M.Averager 2, M.StdDever 2, M.Maxer 2]

basicCSV = TestCase (assertEqual
                      "basic csv stats" "1.0,2.0,3.0\n= 3.0\n\n4.0,5.0,6.0\n= 6.0\n\n7.0,8.0,9.0\n= 9.0\n"
                      (unsafePerformIO (csvTransform  "1,2,3\n4,5,6\n7,8,9" (M.CommandList [M.Grouper 0, M.Grouper 1, M.Averager 2, M.Noop]))))

twoGroupCSV = TestCase (assertEqual
                      "basic csv stats" "1.0,2.0,3.0\n4.0,5.0,3.0\n= 3.5\n\n6.0,7.0,8.0\n9.0,10.0,8.0\n= 8.5\n"
                      (unsafePerformIO (csvTransform  "1,2,3\n4,5,3\n6,7,8\n9,10,8" (M.CommandList [M.Grouper 2, M.Averager 1, M.Noop]))))

tests = TestList [  TestLabel "Basic Command Lists" basicCommandListParsing
                  , TestLabel "Multiple summarizers" multipleSummarizers
                  , TestLabel "Basic CSV stats gathering" basicCSV
                  , TestLabel "Basic two group CSV" twoGroupCSV]

runTests = runTestTT tests

main = runTests

csvTransform :: String -> M.Command -> IO String
csvTransform csv commands =
  do case M.parseStdinCSV csv of
      Left err -> return "error parsing test csv"
      Right parsed -> do
        let res = M.csvToGroupedCSV $ M.csvToPossibleNumbers parsed
        return $ show $ (M.transformCSV (Right commands) res)


----------------------------------------------------------------------------------------------------------------------
-- Properties

instance Arbitrary M.Command where
  arbitrary = elements [ M.CommandList [M.Noop]
                       , M.Noop
                       , M.Summer 1
                       , M.StdDever 1
                       , M.Maxer 1
                       , M.Minner 1
                       , M.Counter 1
                       , M.Averager 1
                       ]

commandAssociativity :: M.Command -> M.Command -> M.Command -> Bool
commandAssociativity = (\a b c -> (a <> b) <> c == a <> (b <> c))

commandIdentity :: M.Command -> Bool
commandIdentity = (\a -> (e <> a == a <> e) && (a <> e == a))
  where e = mempty



