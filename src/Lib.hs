module Lib (
  escapeCreateProcessArg
  ) where

-- | Escape/quote a single argument for Windows CreateProcess
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
