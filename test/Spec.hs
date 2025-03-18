{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Data.String.Interpolate
import Foreign (withArray, peekArray, castPtr, nullPtr, peek, Ptr)
import Foreign.C.Types (CInt(..))
import Lib (escapeCreateProcessArg)
import System.Win32.String (withTString, peekTString)
import System.Win32.Types (LPWSTR, LPCWSTR, BOOL)
import Test.QuickCheck
import UnliftIO.Exception


foreign import stdcall "Windows.h CommandLineToArgvW"
  c_CommandLineToArgvW :: LPCWSTR -> Ptr CInt -> IO (Ptr LPWSTR)

foreign import stdcall "Windows.h LocalFree"
  c_LocalFree :: Ptr a -> IO (Ptr a)

-- | Function to call Windows CommandLineToArgvW, using Win32 for UTF-16 conversion
commandLineToArgvW :: String -> IO [String]
commandLineToArgvW cmdLine = do
  withTString cmdLine $ \cmdLineW ->
    withArray [(0 :: Int)] $ \pNumArgs ->
    bracket (c_CommandLineToArgvW cmdLineW (castPtr pNumArgs)) (void . c_LocalFree) $ \argsPtr -> do
      when (argsPtr == nullPtr) $ error "CommandLineToArgvW failed"
      numArgs <- peek pNumArgs
      peekArray numArgs argsPtr >>= mapM peekTString

testFirstArgQuoting :: String -> Property
testFirstArgQuoting arg = ioProperty $ do
  let quoted = "foo.exe " ++ escapeCreateProcessArg arg
  commandLineToArgvW quoted >>= \case
    ["foo.exe", x] | x == arg -> return True
    ["foo.exe", x] -> return False
    _ -> return False

testMultipleArgsQuoting :: [String] -> Property
testMultipleArgsQuoting args = ioProperty $ do
  let quoted = "foo.exe " ++ unwords (map escapeCreateProcessArg args)
  parsedArgs <- commandLineToArgvW quoted
  commandLineToArgvW quoted >>= \case
    ("foo.exe":xs) -> return (xs == args)
    _ -> return False

genTestString :: Gen String
genTestString = do
  -- Generate strings with a bias toward special characters
  let specialChars = " \"\\'&|<>()^"
  let normalChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

  -- Generate random strings with varied lengths and character distributions
  frequency [
    (1, return "")  -- Empty string
    , (3, listOf1 $ elements normalChars)  -- Normal strings
    , (3, listOf1 $ elements specialChars)  -- Special character strings
    , (5, listOf1 $ frequency  -- Mixed strings with bias toward special chars
        [ (1, elements normalChars)
        , (1, elements specialChars)
        ])
    , (2, do  -- Strings with repeated backslashes
        n <- choose (1, 10)
        return (replicate n '\\'))
    , (2, do  -- Strings with backslashes and quotes
        bs <- listOf1 $ elements "\\"
        return (bs ++ "\""))
    ]

validCommandString :: Gen String
validCommandString = listOf validChar
  where
    validChar = arbitrary `suchThat` (/= '\NUL')

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
