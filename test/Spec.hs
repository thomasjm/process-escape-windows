{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Control.Monad (when)
import Data.List (intercalate)
import Foreign (withArray, peekArray, castPtr, nullPtr, peek, plusPtr, free, Ptr, Word16)
import Foreign.C.String (withCString, peekCString, CString)
import Foreign.C.Types (CInt(..))
import Lib (escapeCreateProcessArg)
import System.Win32.Types (LPWSTR, LPCWSTR)
import Test.QuickCheck
import Test.QuickCheck.Test


-- | Import the CommandLineToArgvW function from Windows API
foreign import stdcall "Windows.h CommandLineToArgvW"
  c_CommandLineToArgvW :: LPCWSTR -> Ptr CInt -> IO (Ptr LPWSTR)

-- | Import LocalFree function to properly clean up memory
foreign import stdcall "Windows.h LocalFree"
  c_LocalFree :: Ptr a -> IO (Ptr a)

-- | Convert String to Windows UTF-16 (to be implemented based on your preferred approach)
stringToUTF16 :: String -> IO LPCWSTR
stringToUTF16 str = undefined -- You'll need to implement UTF-16 conversion

-- | Convert Windows UTF-16 to String
utf16ToString :: LPWSTR -> IO String
utf16ToString ptr = undefined -- You'll need to implement UTF-16 conversion

-- | Function to call Windows CommandLineToArgvW
commandLineToArgvW :: String -> IO [String]
commandLineToArgvW cmdLine = do
  cmdLineW <- stringToUTF16 cmdLine
  numArgsPtr <- withArray [(0 :: Int)] $ \ptr -> do
    argsPtr <- c_CommandLineToArgvW cmdLineW (castPtr ptr)
    when (argsPtr == nullPtr) $ error "CommandLineToArgvW failed"
    numArgs <- peek ptr
    args <- peekArray (fromIntegral numArgs) argsPtr >>= mapM utf16ToString
    _ <- c_LocalFree argsPtr
    return args
  return numArgsPtr

-- | Function to test a single argument
testArgQuoting :: String -> Property
testArgQuoting arg = ioProperty $ do
  -- Quote the argument
  let quoted = escapeCreateProcessArg arg

  -- Call Windows API to parse it back
  parsedArgs <- commandLineToArgvW quoted

  -- Verify it's parsed correctly (accounting for program name in first position)
  return $ length parsedArgs >= 1 &&
          (if null parsedArgs then True else last parsedArgs == arg)

-- | Function to test multiple arguments combined
testArgsQuoting :: [String] -> Property
testArgsQuoting args = ioProperty $ do
  -- Quote each argument and join with spaces
  let quoted = unwords (map escapeCreateProcessArg args)

  -- Call Windows API to parse it back
  parsedArgs <- commandLineToArgvW quoted

  -- Verify all arguments are parsed correctly, accounting for program name
  return $ length parsedArgs == length args + 1 &&
          drop 1 parsedArgs == args

-- | Generate test strings with problematic characters
genTestString :: Gen String
genTestString = do
  -- Generate strings with a bias toward special characters
  let specialChars = " \"\\'&|<>()^"
  let normalChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

  -- Generate random strings with varied lengths and character distributions
  frequency
    [ (1, return "")  -- Empty string
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

-- | Main test suite
main :: IO ()
main = do
  putStrLn "Testing Windows command line argument quoting..."

  -- Test individual argument quoting
  putStrLn "Testing single argument quoting:"
  quickCheckWith stdArgs {maxSuccess = 1000} $
    forAll genTestString testArgQuoting

  -- Test multiple arguments quoting
  putStrLn "Testing multiple arguments quoting:"
  quickCheckWith stdArgs {maxSuccess = 500} $
    forAll (listOf1 genTestString) testArgsQuoting

  -- Test specific edge cases
  putStrLn "Testing known edge cases:"
  let edgeCases =
        [ ""                        -- Empty string
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

  mapM_ (\arg -> do
      putStrLn $ "Testing: " ++ show arg
      quoted <- return $ escapeCreateProcessArg arg
      putStrLn $ "Quoted as: " ++ quoted
      parsed <- commandLineToArgvW quoted
      putStrLn $ "Parsed: " ++ show parsed
      putStrLn $ "Result: " ++ if length parsed >= 1 && (if null parsed then True else last parsed == arg)
                                then "PASS" else "FAIL"
      putStrLn ""
    ) edgeCases
