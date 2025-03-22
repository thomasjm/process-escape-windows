
 module Main (main) where

import Control.Monad
import Data.String.Interpolate
import Lib (escapeCreateProcessArg)
import Test.QuickCheck
import TestLib.Gen
import TestLib.Props


main :: IO ()
main = do
  putStrLn "Testing known edge cases"
  forM_ testCases $ \arg -> do
    let quoted = "foo.exe " ++ escapeCreateProcessArg arg
    commandLineToArgvW quoted >>= \case
      ["foo.exe", x] | x == arg -> return ()
      ["foo.exe", x] -> putStrLn [i|Failure: #{show arg} -> #{show quoted} -> #{show x}\n|]
      xs -> putStrLn [i|Failure: unexpected parsed value: #{xs}|]

  putStrLn "\n"

  let n = 10000

  putStrLn "Testing single argument quoting"
  quickCheckWith (stdArgs {maxSuccess = n}) $
    forAll genTestString testFirstArgQuoting

  putStrLn "\n"

  putStrLn "Testing multiple arguments quoting:"
  quickCheckWith (stdArgs {maxSuccess = n}) $
    forAll (listOf1 genTestString) testMultipleArgsQuoting

  putStrLn "\n"

  putStrLn "Running full QuickCheck for first arg"
  quickCheckWith (stdArgs {maxSuccess = n}) $
    forAll validCommandString testFirstArgQuoting

  putStrLn "\n"

  putStrLn "Running full QuickCheck for multi arg"
  quickCheckWith (stdArgs {maxSuccess = n}) $
    forAll (listOf1 validCommandString) testMultipleArgsQuoting

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
