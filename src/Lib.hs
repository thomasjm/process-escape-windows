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
-- 4. N backslashes followed by a double quote ? 2N+1 backslashes and a double quote
-- 5. N backslashes not followed by a double quote ? N backslashes
-- 6. A double quote within a quoted argument is represented by \"
escapeCreateProcessArg :: String -> String
escapeCreateProcessArg arg
  | not (needsQuoting arg) = arg
  | otherwise = "\"" ++ escapeWithEndQuote arg ++ "\""
  where
    -- Check if an argument needs quoting
    needsQuoting :: String -> Bool
    needsQuoting s = null s || any needsQuote s

    needsQuote :: Char -> Bool
    needsQuote c = c `elem` [' ', '\t', '"', '\'', '(', ')', '<', '>', '&', '|', '^', '%']

    -- Main escaping function that accounts for the ending quote
    escapeWithEndQuote :: String -> String
    escapeWithEndQuote s =
      let (revProcessed, needFinalEscape) = processString [] s
      in if needFinalEscape
         then reverse (escapeTrailingBackslashes revProcessed)
         else reverse revProcessed

    -- Process the string character by character, tracking backslashes
    processString :: String -> String -> (String, Bool)
    processString acc [] = (acc, True)  -- End of string, will need final escaping if backslashes
    processString acc ('"':xs) = processString ('\"':'\\':acc) xs  -- Rule 6
    processString acc xs =
      let (backslashes, rest) = span (== '\\') xs
          bsCount = length backslashes
      in case rest of
          '"':rest' ->
            let escapedBS = replicate (2 * bsCount + 1) '\\' ++ "\""
            in processString (reverse escapedBS ++ acc) rest'  -- Rule 4
          [] ->
            -- End of string with backslashes, will handle in escapeTrailingBackslashes
            (replicate bsCount '\\' ++ acc, bsCount > 0)
          (c:cs) ->
            processString (c : replicate bsCount '\\' ++ acc) cs  -- Rule 5

    -- Handle trailing backslashes that will be followed by the closing quote
    escapeTrailingBackslashes :: String -> String
    escapeTrailingBackslashes s =
      let (backslashes, rest) = span (== '\\') s
          bsCount = length backslashes
      in replicate (2 * bsCount) '\\' ++ rest  -- Double the backslashes for the final quote
