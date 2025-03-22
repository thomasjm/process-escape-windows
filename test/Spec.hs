
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
    describe "With fixed executable name (foo.exe)" $ do
      describe "Test strings (weighted towards special chars, backslashes, quotes)" $ do
        prop "single argument" $ forAll genTestString (\x -> executableAndArgsWork "foo.exe" [x])
        prop "multi argument" $ forAll (listOf1 genTestString) (\xs -> executableAndArgsWork "foo.exe" xs)

      describe "Arbitrary strings" $ do
        prop "single argument" $ forAll stringWithoutNulls (\x -> executableAndArgsWork "foo.exe" [x])
        prop "multi argument" $ forAll (listOf1 stringWithoutNulls) (\xs -> executableAndArgsWork "foo.exe" xs)

    describe "With random executable name" $ do
      describe "Test strings (weighted towards special chars, backslashes, quotes)" $ do
        prop "Executable only" $ forAll genTestString (\x -> executableAndArgsWork x [])
        prop "Executable + args" $ forAll (listOf1 genTestString) (\(x:xs) -> executableAndArgsWork x xs)

      describe "Arbitrary strings" $ do
        prop "Executable only" $ forAll stringWithoutNulls (\x -> executableAndArgsWork x [])
        prop "Executable + args" $ forAll (listOf1 stringWithoutNulls) (\(x:xs) -> executableAndArgsWork x xs)

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
