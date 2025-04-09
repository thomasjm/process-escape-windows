module Lib (
  escapeCreateProcessArg0
  , escapeCreateProcessArg
  , escapeCreateProcessArgForCmd
  ) where


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

-- | There is also a special escaping scheme you need to using when passing args
-- to a batch file (.bat or .cmd), in order to avoid the BatBadBut vulnerability.
--
-- https://flatt.tech/research/posts/batbadbut-you-cant-securely-execute-commands-on-windows/
-- https://github.com/haskell/security-advisories/blob/0ca84023348231a44fac0ee943cca5437ef711a5/advisories/hackage/process/HSEC-2024-0003.md
--
-- The code below follows the escaping algorithm described in the first link.
escapeCreateProcessArgForCmd :: String -> String
escapeCreateProcessArgForCmd arg = escape $ escapeCreateProcessArg arg
  where
    escape ('(':xs) = '^' : '(' : escape xs
    escape (')':xs) = '^' : ')' : escape xs
    escape ('%':xs) = '^' : '%' : escape xs
    escape ('!':xs) = '^' : '!' : escape xs
    escape ('^':xs) = '^' : '^' : escape xs
    escape ('"':xs) = '^' : '"' : escape xs
    escape ('<':xs) = '^' : '<' : escape xs
    escape ('>':xs) = '^' : '>' : escape xs
    escape ('&':xs) = '^' : '&' : escape xs
    escape ('|':xs) = '^' : '|' : escape xs
    escape (x:xs) = x : escape xs
    escape [] = []

  -- | not (needsQuoting arg) = arg
  -- | otherwise = "\"" ++ escape arg ++ "\""
  -- where
  --   -- Check if an argument needs quoting
  --   needsQuoting :: String -> Bool
  --   needsQuoting s = null s || any (`elem` specialChars) s

  --   specialChars :: [Char]
  --   specialChars = [' ', '\t', '"', '\'', '(', ')', '<', '>', '&', '|', '^', '%']

  --   escape :: String -> String
  --   -- Replace percent sign (%) with %%cd:~,%.
  --   escape ('%':xs) = "%%cd:~,%" ++ escape xs
  --   -- Replace the backslash (\) in front of the double quote (") with two backslashes (\\).
  --   escape ('\\':xs@('"':_)) = '\\' : '\\' : escape xs
  --   -- Replace the double quote (") with two double quotes ("").
  --   escape ('"':xs) = '"' : '"' : escape xs
  --   -- Remove newline characters (\n).
  --   escape ('\n':xs) = escape xs
  --   -- All other characters are passed through normally
  --   escape (c:xs) = c : escape xs
  --   escape [] = []
