
 module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.String.Interpolate
import Lib (escapeCreateProcessArg)
import Test.QuickCheck
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Gen
import TestLib.Props


tests :: TopSpec
tests = do
  it "basic cases" $ do
    forM_ testCases $ \arg -> do
      let quoted = "foo.exe " ++ escapeCreateProcessArg arg
      liftIO (commandLineToArgvW quoted) >>= \case
        ["foo.exe", x] | x == arg -> return ()
        ["foo.exe", x] -> expectationFailure [i|Failure: #{show arg} -> #{show quoted} -> #{show x}\n|]
        xs -> expectationFailure [i|Failure: unexpected parsed value: #{xs}|]


  introduceQuickCheck' (stdArgs { maxSuccess = 10000 }) $ do
    describe "Test strings (weighted towards special chars, backslashes, quotes)" $ do
      prop "single argument" $ forAll genTestString testFirstArgQuoting
      prop "multi argument" $ forAll (listOf1 genTestString) testMultipleArgsQuoting

    describe "Arbitrary strings" $ do
      prop "single argument" $ forAll stringWithoutNulls testFirstArgQuoting
      prop "multi argument" $ forAll (listOf1 stringWithoutNulls) testMultipleArgsQuoting


testCases :: [String]
testCases = [
  ""                          -- Empty string
  , "simple"                  -- Simple string, no quoting needed
  , "has space"               -- Contains spaces
  , "has\"quote"              -- Contains quotes
  , "\\"                      -- Single backslash
  , "\\\\"                    -- Two backslashes
  , "\\\""                    -- Backslash followed by quote
  , "ends with \\"            -- Ends with backslash
  , "\\arg with space"        -- Backslash at start with spaces
  , "a\\\\b c"                -- Backslashes in middle with spaces
  , "a\\\\\"b c"              -- Backslashes followed by quote
  , "\"quoted already\""      -- Already quoted
  , "with & special | chars"  -- With shell special chars
  ]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
