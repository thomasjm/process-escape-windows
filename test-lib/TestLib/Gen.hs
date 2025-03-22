
module TestLib.Gen (genTestString) where

import Test.QuickCheck


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
