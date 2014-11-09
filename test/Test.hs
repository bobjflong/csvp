
module Test where

import qualified Main                as M
import qualified PossibleNumber      as P
import qualified Summarizer          as S

import           Control.Lens        hiding (elements)
import           Control.Monad.State
import           Data.Monoid
import           System.IO.Unsafe
import           Test.HUnit
import           Test.QuickCheck
import           Text.Parsec.Error

{-
  Command parsing
-}
basicCommandListParsing = TestCase (assertEqual "basic command list" parsed fromStr)
  where fromStr = (^?_Right) $ M.parseCommandsFromArgs "group 0; group 1; avg 2;"
        parsed  = Just $ M.CommandList [M.Grouper 0, M.Grouper 1, M.Averager 2]

multipleSummarizers = TestCase (assertEqual "multiple summarizers" parsed fromStr)
  where fromStr = (^?_Right) $ M.parseCommandsFromArgs $ "group 0; avg 2; stddev 2; max 2;"
        parsed  = Just $ M.CommandList [M.Grouper 0, M.Averager 2, M.StdDever 2, M.Maxer 2]

{-
  Group/Summary calculation
-}
basicCSV = TestCase (
  assertEqual
  "basic csv stats with subgroups"
  "1.0,2.0,3.0\n= 3.0\n\n4.0,5.0,6.0\n= 6.0\n\n7.0,8.0,9.0\n= 9.0\n"
  (unsafePerformIO
    (csvTransform  "1,2,3\n4,5,6\n7,8,9" (M.CommandList [M.Grouper 0, M.Grouper 1, M.Averager 2, M.Noop]))))

twoGroupCSV = TestCase (
  assertEqual
  "basic csv stats"
  "1.0,2.0,3.0\n4.0,5.0,3.0\n= 3.5\n\n6.0,7.0,8.0\n9.0,10.0,8.0\n= 8.5\n"
  (unsafePerformIO
    (csvTransform  "1,2,3\n4,5,3\n6,7,8\n9,10,8" (M.CommandList [M.Grouper 2, M.Averager 1, M.Noop]))))

testCases = TestList [  TestLabel "Basic Command Lists" basicCommandListParsing
                  , TestLabel "Multiple summarizers" multipleSummarizers
                  , TestLabel "Basic CSV stats gathering" basicCSV
                  , TestLabel "Basic two group CSV" twoGroupCSV]

{-
  Transform the CSV using csvp.Main
-}
csvTransform :: String -> M.Command -> IO String
csvTransform csv commands =
  do case P.parseStdinCSV csv of
      Left err -> return "error parsing test csv"
      Right parsed -> do
        let res = S.csvToGroupedCSV $ P.csvToPossibleNumbers parsed
        return $ show $ (fromEither $ M.transformCSV (Right commands) res)

runTests = runTestTT testCases

{-
  Properties
-}
instance Arbitrary M.Command where
  arbitrary = do index <- (arbitrary :: Gen Int)
                 elements [ M.CommandList [M.Noop]
                       , M.Noop
                       , M.Summer index
                       , M.StdDever index
                       , M.Maxer index
                       , M.Minner index
                       , M.Counter index
                       , M.Averager index
                       ]

{-
  Associativity for the Command Monoid
-}
commandAssociativity :: M.Command -> M.Command -> M.Command -> Bool
commandAssociativity = (\a b c -> (a <> b) <> c == a <> (b <> c))

{-
  Identity for the Command Monoid
-}
commandIdentity :: M.Command -> Bool
commandIdentity = (\a -> (e <> a == a <> e) && (a <> e == a))
  where e = mempty

{-
  Concatenation for the Command Monoid
-}
commandConcat :: [M.Command] -> Bool
commandConcat x = mconcat x == foldr mappend mempty x

runPropertyChecks = quickCheck commandAssociativity
                    >> quickCheck commandIdentity
                    >> quickCheck commandConcat

main = do greenColour
          putStrLn "\nTests\n"
          normalColour
          runTests
          greenColour
          putStrLn "\nProperties\n"
          normalColour
          runPropertyChecks

  where greenColour = putStr "\x1b[32m"
        normalColour = putStr "\x1b[0m"

fromEither (Right x) = x

