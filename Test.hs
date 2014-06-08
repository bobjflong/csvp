
module Test where

import Main

import Test.HUnit
import Control.Lens

basicCommandListParsing = TestCase (assertEqual "basic command list" parsed fromStr)
  where fromStr = mapped %~ (^?_Right) $ parseCommandsFromArgs $ ["group 0; group 1; avg 2;"]
        parsed  = [Just $ CommandList [Grouper 0, Grouper 1, Averager 2]]

multipleSummarizers = TestCase (assertEqual "multiple summarizers" parsed fromStr)
  where fromStr = mapped %~ (^?_Right) $ parseCommandsFromArgs $ ["group 0; avg 2; stddev 2; max 2;"]
        parsed  = [Just $ CommandList [Grouper 0, Averager 2, StdDever 2, Maxer 2]] 

tests = TestList [TestLabel "Basic Command Lists" basicCommandListParsing, TestLabel "Multiple summarizers" multipleSummarizers]

runTests = runTestTT tests
