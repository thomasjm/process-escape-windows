{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

 module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.String.Interpolate
import Lib
import Test.QuickCheck
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Gen
import TestLib.Props


tests :: TopSpec
tests = introduceQuickCheck' (stdArgs { maxSuccess = 1000 }) $ do
  describe "foo.exe <args> (tested using CommandLineToArgvW)" $ do
    runTests (executableAndArgsWork' "foo.exe") (executableAndArgsWork "foo.exe")

  describe "foo.exe <args> for OLD VERSION from System.Process (tested using CommandLineToArgvW)" $ do
    runTests (executableAndArgsWorkUsingOldFunction' "foo.exe") (executableAndArgsWorkUsingOldFunction "foo.exe")

  describe "cmd.exe /c child.exe <args> (tested with (readCreateProcessWithExitCode (shell (...))))" $ do
    runTests argsWorkUsingCmdExeWithCProgram' argsWorkUsingCmdExeWithCProgram

  describe "cmd.exe /c child.bat <args> (tested with (readCreateProcessWithExitCode (shell (...))))" $ do
    runTests argsWorkUsingCmdExeWithBatchFile' argsWorkUsingCmdExeWithBatchFile

  describe "escapeCreateProcessArg0 cases" $ do
    let testArg0Cases :: [String]
        testArg0Cases = [
          [i|C:\\Program Files\\Foo\\bar.txt|]
          ]

    forM_ testArg0Cases $ \exe -> do
      it exe $ liftIO $ executableAndArgsWork' exe []

  describe "<random exe name> <args> (tested using CommandLineToArgvW)" $ do
    prop "Executable only (strings without forbidden path chars)" $
      forAll stringWithoutInvalidWindowsPathChars (\x -> executableAndArgsWork x [])

    prop "Executable + test string args (weighted towards special chars, backslashes, quotes)" $
      forAll stringWithoutInvalidWindowsPathChars $ \exe ->
        forAll (listOf1 genTestString) $ \args ->
          executableAndArgsWork exe args

    prop "Executable + arbitrary args" $ forAll stringWithoutInvalidWindowsPathChars $ \exe ->
      forAll (listOf1 stringWithoutInvalidWindowsPathChars) $ \args ->
        executableAndArgsWork exe args

runTests ioCheck propCheck = do
  it "single arg cases" $ do
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
      liftIO $ ioCheck [arg]

  it "multi arg cases" $ do
    liftIO $ ioCheck ["a", "b"]
    liftIO $ ioCheck ["a\"", "b"]

  it "BatBadBut" $ do
    liftIO $ ioCheck ["\"&calc.exe"]

  describe "Test strings (weighted towards special chars, backslashes, quotes)" $ do
    prop "single argument" $ forAll genTestString (\x -> propCheck [x])
    prop "multi argument" $ forAll (listOf1 genTestString) (\xs -> propCheck xs)

  describe "Arbitrary strings" $ do
    prop "single argument" $ forAll stringWithoutNulls (\x -> propCheck [x])
    prop "multi argument" $ forAll (listOf1 stringWithoutNulls) (\xs -> propCheck xs)


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
