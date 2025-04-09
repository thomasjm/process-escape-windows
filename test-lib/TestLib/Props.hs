{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module TestLib.Props (
  commandLineToArgvW

  , executableAndArgsWork

  , stringWithoutNulls
  , stringWithoutInvalidWindowsPathChars

  , argsWorkUsingCmd
  , argsWorkUsingCmd'

  , executableAndArgsWorkUsingOldFunction
  ) where

import Control.Monad
import Data.Char (ord)
import Data.Function ((&))
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import Foreign (withArray, peekArray, castPtr, nullPtr, peek, Ptr)
import Foreign.C.Types (CInt(..))
import Lib
import System.Exit
import System.FilePath
import System.Win32.String (withTString, peekTString)
import System.Win32.Types (LPWSTR, LPCWSTR)
import Test.QuickCheck
import Test.Sandwich
import UnliftIO.Directory hiding (executable)
import UnliftIO.Exception
import UnliftIO.Process hiding (cwd)


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

argsWorkUsingCmd :: [String] -> Property
argsWorkUsingCmd = ioProperty . argsWorkUsingCmd'

argsWorkUsingCmd' :: [String] -> IO ()
argsWorkUsingCmd' args = do
  cwd <- getCurrentDirectory
  let testBat = cwd </> "test-assets" </> "test-multi.bat"
  doesFileExist testBat >>= (`shouldBe` True)

  (exitCode, sout, serr) <- readCreateProcessWithExitCode (shell (L.unwords (testBat : fmap escapeCreateProcessArgForCmd args))) ""
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure n -> expectationFailure [i|Process exited with code #{n}. Stdout: #{sout}. Stderr: #{serr}.|]

  let ls = sout
         & T.pack
         & T.splitOn "\n"
         & L.init -- Remove trailing newline
         & fmap T.unpack
  ls `shouldBe` args

-- * Old version in System.Process

executableAndArgsWorkUsingOldFunction :: String -> [String] -> Property
executableAndArgsWorkUsingOldFunction executable args = ioProperty $ do
  let quoted = unwords (escapeCreateProcessArg0 executable : fmap translateInternal args)
  commandLineToArgvW quoted >>= \case
    (exe:rest) | exe == executable && rest == args -> return True
    _ -> return False
  where
    -- | This is the old version in System.Process, as of 23c54d7c0a3dc3e6e3da6b427f2ba332cb9bae71
    translateInternal :: String -> String
    translateInternal xs = '"' : snd (foldr escape (True,"\"") xs)
      where escape '"'  (_,     str) = (True,  '\\' : '"'  : str)
            escape '\\' (True,  str) = (True,  '\\' : '\\' : str)
            escape '\\' (False, str) = (False, '\\' : str)
            escape c    (_,     str) = (False, c : str)

-- * Util

escapeCmdAndArgs :: String -> [String] -> String
escapeCmdAndArgs exe args = unwords (escapeCreateProcessArg0 exe : fmap escapeCreateProcessArg args)
