{-# LANGUAGE ForeignFunctionInterface #-}

module TestLib.Props (
  commandLineToArgvW

  , executableAndArgsWork

  , stringWithoutNulls
  , stringWithoutInvalidWindowsPathChars
  ) where

import Control.Monad
import Data.Char (ord)
import Foreign (withArray, peekArray, castPtr, nullPtr, peek, Ptr)
import Foreign.C.Types (CInt(..))
import Lib (escapeCmdAndArgs)
import System.Win32.String (withTString, peekTString)
import System.Win32.Types (LPWSTR, LPCWSTR)
import Test.QuickCheck
import UnliftIO.Exception


foreign import ccall "Windows.h CommandLineToArgvW"
  c_CommandLineToArgvW :: LPCWSTR -> Ptr CInt -> IO (Ptr LPWSTR)

foreign import ccall "Windows.h LocalFree"
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

executableAndArgsWork :: String -> [String] -> Property
executableAndArgsWork executable args = ioProperty $ do
  let quoted = escapeCmdAndArgs executable args
  commandLineToArgvW quoted >>= \case
    (exe:rest) | exe == executable && rest == args -> return True
    _ -> return False

stringWithoutNulls :: Gen String
stringWithoutNulls = listOf validChar
  where
    validChar = arbitrary `suchThat` (/= '\NUL')

stringWithoutInvalidWindowsPathChars :: Gen String
stringWithoutInvalidWindowsPathChars = listOf1 validChar
  where
    validChar = arbitrary `suchThat` (not . isWindowsForbiddenPathChar)

    isWindowsForbiddenPathChar :: Char -> Bool
    isWindowsForbiddenPathChar x =
      x `elem` ['<', '>', ':', '"', '/', '\\', '|', '?', '*'] -- printable
      || (ord x >= 0 && ord x <= 31) -- non-printable
