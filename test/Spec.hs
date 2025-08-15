{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

 module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.String.Interpolate
import System.Win32.Console
import Test.QuickCheck
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Gen
import TestLib.Props


tests :: TopSpec
tests = introduceQuickCheck' (stdArgs { maxSuccess = 100 }) $ do
  describe "foo.exe <args> (tested using CommandLineToArgvW)" $ do
    runTests (executableAndArgsWork "foo.exe")

  describe "foo.exe <args> for OLD VERSION from System.Process (tested using CommandLineToArgvW)" $ do
    runTests (executableAndArgsWorkUsingOldFunction "foo.exe")

  describe "cmd.exe /c child.exe <args> (tested with (readCreateProcessWithExitCode (shell (...))))" $ do
    runTests' stringWithoutNullsOrNewlines argsWorkUsingCmdExeWithCProgram

  describe "child.bat <args> (tested with (readCreateProcessWithExitCode (proc (...))))" $ do
    runTests argsWorkUsingRawCommandBatchFile

  describe "escapeCreateProcessArg0 cases" $ do
    let testArg0Cases :: [String]
        testArg0Cases = [
          [i|C:\\Program Files\\Foo\\bar.txt|]
          ]

    forM_ testArg0Cases $ \exe -> do
      it exe $ liftIO $ executableAndArgsWork exe []

  describe "<random exe name> <args> (tested using CommandLineToArgvW)" $ do
    prop "Executable only (strings without forbidden path chars)" $
      forAll stringWithoutInvalidWindowsPathChars (\x -> ioProperty $ executableAndArgsWork x [])

    prop "Executable + test string args (weighted towards special chars, backslashes, quotes)" $
      forAll stringWithoutInvalidWindowsPathChars $ \exe ->
        forAll (listOf1 genTestString) $ \args ->
          ioProperty $ executableAndArgsWork exe args

    prop "Executable + arbitrary args" $ forAll stringWithoutInvalidWindowsPathChars $ \exe ->
      forAll (listOf1 stringWithoutInvalidWindowsPathChars) $ \args ->
        ioProperty $ executableAndArgsWork exe args

runTests :: (MonadIO m, HasQuickCheckContext ctx) => ([String] -> IO ()) -> SpecFree ctx m ()
runTests = runTests' stringWithoutNulls

runTests' :: (MonadIO m, HasQuickCheckContext ctx) => Gen String -> ([String] -> IO ()) -> SpecFree ctx m ()
runTests' arbitraryString ioCheck = do
  describe "single arg cases" $ do
    let testArgCases :: [String]
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
          , "\"&calc.exe"             -- BatBadBut
          ]

    forM_ testArgCases $ \arg -> do
      it [i|#{arg}|] $
        liftIO $ ioCheck [arg]

  it "multi arg cases" $ do
    liftIO $ ioCheck ["a", "b"]
    liftIO $ ioCheck ["a\"", "b"]

  it "BatBadBut" $ do
    liftIO $ ioCheck ["\"&calc.exe"]

  describe "Test strings (weighted towards special chars, backslashes, quotes)" $ do
    prop "single argument" $ forAll genTestString (\x -> ioProperty $ ioCheck [x])
    prop "multi argument" $ forAll (listOf1 genTestString) (\xs -> ioProperty $ ioCheck xs)

  describe "Arbitrary strings" $ do
    prop "single argument" $ forAll arbitraryString (\x -> ioProperty $ ioCheck [x])
    prop "multi argument" $ forAll (listOf1 arbitraryString) (\xs -> ioProperty $ ioCheck xs)


main :: IO ()
main = do
  setConsoleCP 65001
  setConsoleOutputCP 65001

  runSandwichWithCommandLineArgs defaultOptions tests
