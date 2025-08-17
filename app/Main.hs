{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.Interpolate
import System.Directory
import System.FilePath
import System.Process


main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let childBat = cwd </> "test-assets" </> "child.bat"

  putStrLn [i|Running #{childBat}|]
  (exitCode, sout, serr) <- readCreateProcessWithExitCode (proc childBat ["a\"b"]) ""

  putStrLn [i|exitCode: '#{exitCode}'|]
  putStrLn [i|sout: '#{sout}'|]
  putStrLn [i|serr: '#{serr}'|]
