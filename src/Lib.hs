module Lib (
  escapeCreateProcessArg
  ) where

-- | Escape/quote a single argument for Windows CreateProcess
--
-- This follows the escaping rules described in Microsoft's documentation:
-- https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw
--
-- The rules are complex:
-- 1. If the argument doesn't contain spaces or quotes, it remains unquoted
-- 2. Otherwise, the argument is surrounded by double quotes
-- 3. Backslashes are only significant when they precede a double quote
-- 4. N backslashes followed by a double quote → 2N+1 backslashes and a double quote
-- 5. N backslashes not followed by a double quote → N backslashes
-- 6. A double quote within a quoted argument is represented by \"
escapeCreateProcessArg :: String -> String
escapeCreateProcessArg arg
  | not (needsQuoting arg) = arg
  | otherwise = "\"" ++ escape arg ++ "\""
  where
    -- Check if an argument needs quoting
    needsQuoting :: String -> Bool
    needsQuoting s = null s || any needsQuote s

    needsQuote :: Char -> Bool
    needsQuote c = c == ' ' || c == '\t' || c == '\"' || c == '\''
                || c == '(' || c == ')' || c == '<' || c == '>'
                || c == '&' || c == '|' || c == '^' || c == '%'

    -- Handle the complex escaping rules
    escape :: String -> String
    escape [] = []
    escape ('"':xs) = "\\\"" ++ escape xs  -- Rule 6
    escape xs =
      let (backslashes, rest) = span (== '\\') xs
          bsCount = length backslashes
      in case rest of
          '"':rest' -> replicate (2 * bsCount + 1) '\\' ++ "\"" ++ escape rest'  -- Rule 4
          _         -> replicate bsCount '\\' ++
                        case rest of
                          []     -> []
                          (c:cs) -> c : escape cs  -- Rule 5
