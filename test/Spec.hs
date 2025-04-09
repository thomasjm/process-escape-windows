{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

 module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import Lib
import System.FilePath
import Test.QuickCheck
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Gen
import TestLib.Props
import UnliftIO.Directory
import UnliftIO.Process hiding (cwd)


tests :: TopSpec
tests = do
  describe "escapeCreateProcessArg0" $ do
    let testArg0Cases :: [String]
        testArg0Cases = [
          -- ""
          -- , " "
          [i|C:\\Program Files\\Foo\\bar.txt|]
          ]

    forM_ testArg0Cases $ \arg -> do
      it arg $ do
        let quoted = escapeCreateProcessArg0 arg
        liftIO (commandLineToArgvW quoted) >>= \case
          [x]
            | x == arg -> return ()
            | otherwise -> expectationFailure [i|Failure: #{show arg} -> #{show quoted} -> #{show x}\n|]
          xs -> expectationFailure [i|Failure: #{show arg} -> #{show quoted} -> too many parsed values: #{xs}|]

  describe "escapeCreateProcessArg cases" $ do
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

      describe "Old version from System.Process" $ do
        describe "Test strings (weighted towards special chars, backslashes, quotes)" $ do
          prop "single argument" $ forAll genTestString (\x -> executableAndArgsWorkUsingOldFunction "foo.exe" [x])
          prop "multi argument" $ forAll (listOf1 genTestString) (\xs -> executableAndArgsWorkUsingOldFunction "foo.exe" xs)

        describe "Arbitrary strings" $ do
          prop "single argument" $ forAll stringWithoutNulls (\x -> executableAndArgsWorkUsingOldFunction "foo.exe" [x])
          prop "multi argument" $ forAll (listOf1 stringWithoutNulls) (\xs -> executableAndArgsWorkUsingOldFunction "foo.exe" xs)

    describe "With random executable name" $ do
      prop "Executable only (strings without forbidden path chars)" $ forAll stringWithoutInvalidWindowsPathChars (\x -> executableAndArgsWork x [])

      prop "Executable + test string args (weighted towards special chars, backslashes, quotes)" $ forAll stringWithoutInvalidWindowsPathChars $ \exe ->
        forAll (listOf1 genTestString) $ \args ->
          executableAndArgsWork exe args

      prop "Executable + arbitrary args" $ forAll stringWithoutInvalidWindowsPathChars $ \exe ->
        forAll (listOf1 stringWithoutInvalidWindowsPathChars) $ \args ->
          executableAndArgsWork exe args

  describe ".bat/.cmd escaping" $ do
    it "simple" $ do
      liftIO $ argsWorkUsingCmd' ["a", "b"]

    it "stray quote" $ do
      liftIO $ argsWorkUsingCmd' ["a\"", "b"]

    -- it "BatBadBut" $ do
    --   liftIO $ argsWorkUsingCmd' ["\"&calc.exe"]

    -- it "test.bat works" $ do
    --   cwd <- getCurrentDirectory
    --   let testBat = cwd </> "test-assets" </> "test.bat"
    --   doesFileExist testBat >>= (`shouldBe` True)

    --   let args = fmap escapeCreateProcessArgForCmd ["a", "b"]

    --   (T.splitOn "\n" . T.strip . T.pack) <$> readCreateProcess (shell (L.unwords (testBat : args))) ""
    --     >>= (`shouldBe` [
    --             "0: " <> T.pack testBat,
    --             "1: a",
    --             "2: b"
    --             ])

    -- it "test.bat doesn't permit BatBadBut exploit" $ do
    --   cwd <- getCurrentDirectory
    --   let testBat = cwd </> "test-assets" </> "test.bat"
    --   doesFileExist testBat >>= (`shouldBe` True)

    --   let args = fmap escapeCreateProcessArgForCmd ["\"&calc.exe"]

    --   (T.splitOn "\n" . T.strip . T.pack) <$> readCreateProcess (shell (L.unwords (testBat : args))) ""
    --     >>= (`shouldBe` [
    --             "0: " <> T.pack testBat,
    --             "1: a",
    --             "2: b"
    --             ])

    -- it "test.bat works" $ do
    --   cwd <- getCurrentDirectory
    --   let testBat = cwd </> "test-assets" </> "test.bat"
    --   doesFileExist testBat >>= (`shouldBe` True)

    --   (T.splitOn "\n" . T.strip . T.pack) <$> readCreateProcess (proc testBat ["a", "b"]) ""
    --     >>= (`shouldBe` [
    --             "0: " <> T.pack testBat,
    --             "1: a",
    --             "2: b"
    --             ])

    -- it "test.bat doesn't permit BatBadBut exploit" $ do
    --   cwd <- getCurrentDirectory
    --   let testBat = cwd </> "test-assets" </> "test.bat"
    --   doesFileExist testBat >>= (`shouldBe` True)

    --   (T.splitOn "\n" . T.strip . T.pack) <$> readCreateProcess (proc testBat ["\"&calc.exe"]) ""
    --     >>= (`shouldBe` [
    --             "0: " <> T.pack testBat,
    --             "1: a",
    --             "2: b"
    --             ])


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
