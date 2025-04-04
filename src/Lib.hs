module Lib (
  escapeCmdAndArgs
  , escapeCreateProcessArg0
  , escapeCreateProcessArg
  ) where


escapeCmdAndArgs :: String -> [String] -> String
escapeCmdAndArgs exe args = unwords (escapeCreateProcessArg0 exe : fmap escapeCreateProcessArg args)

-- | Escape the *first* argument for Windows CreateProcess.
-- For subsequent arguments, see 'escapeCreateProcessArg'.
--
-- The first argument is parsed differently than subsequent arguments. It must be a valid
-- Windows path. To ensure it's escaped properly, we do two things:
-- a) Strip out quotes from the path, since quotes are forbidden in Windows paths
--    (see https://stackoverflow.com/a/31976060)
-- b) If the resulting string contains any whitespace, wrap it in double quotes. Otherwise,
--    leave it as-is.
escapeCreateProcessArg0 :: String -> String
escapeCreateProcessArg0 exe
  | not (hasWhitespace exe) = exeWithoutForbiddenChars
  | otherwise = "\"" ++ exeWithoutForbiddenChars ++ "\""
  where
    exeWithoutForbiddenChars = filter (not . (== '"')) exe

    hasWhitespace = any (`elem` " \t")

-- | Escape a single argument for Windows CreateProcess.
-- (Not the first argument! For argv[0], see 'escapeCreateProcessArg0'.)
--
-- This follows the escaping rules described in Microsoft's documentation:
-- https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw
escapeCreateProcessArg :: String -> String
escapeCreateProcessArg arg
  | not (needsQuoting arg) = arg
  | otherwise = "\"" ++ escape arg True ++ "\""
  where
    -- Check if an argument needs quoting
    needsQuoting :: String -> Bool
    needsQuoting s = null s || any (`elem` specialChars) s

    specialChars :: [Char]
    specialChars = [' ', '\t', '"', '\'', '(', ')', '<', '>', '&', '|', '^', '%']

    -- Escape the string, with a flag indicating if we're at the end
    -- (meaning the next character would be the closing quote)
    escape :: String -> Bool -> String
    escape [] _ = []
    escape ('"':xs) endsWithQuote = "\\\"" ++ escape xs endsWithQuote
    escape xs endsWithQuote =
      let (backslashes, rest) = span (== '\\') xs
          bsCount = length backslashes
      in case rest of
          -- If backslashes are followed by a quote, they need to be doubled plus one
          '"':rest' -> replicate (2 * bsCount + 1) '\\' ++ "\"" ++ escape rest' endsWithQuote

          -- If we're at the end of the string, backslashes need to be doubled
          [] | endsWithQuote -> replicate (2 * bsCount) '\\'

          -- Otherwise, backslashes remain as is
          [] -> replicate bsCount '\\'
          (c:cs) -> replicate bsCount '\\' ++ c : escape cs endsWithQuote
