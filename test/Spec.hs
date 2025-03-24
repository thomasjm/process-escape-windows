{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

 module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.String.Interpolate
import Lib (escapeCreateProcessArg0, escapeCreateProcessArg)
import Test.QuickCheck
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Gen
import TestLib.Props


tests :: TopSpec
tests = do
  describe "escapeCreateProcessArg0" $ do
    forM_ testArg0Cases $ \arg -> do
      it arg $ do
        let quoted = escapeCreateProcessArg0 arg
        liftIO (commandLineToArgvW quoted) >>= \case
          [x]
            | x == arg -> return ()
            | otherwise -> expectationFailure [i|Failure: #{show arg} -> #{show quoted} -> #{show x}\n|]
          xs -> expectationFailure [i|Failure: #{show arg} -> #{show quoted} -> too many parsed values: #{xs}|]

  describe "escapeCreateProcessArg cases" $ do
    forM_ testArgCases $ \arg -> do
      it arg $ do
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

testArg0Cases :: [String]
testArg0Cases = [
  -- ""                          -- Empty string (disabled since this causes commandLineToArgvW to return the current process name)
  -- , " "                       -- Single space
  [i|\\"|]                       -- Single backslash plus quote
  , [i|a "|]
  ]

testArgCases :: [String]
testArgCases = [
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
