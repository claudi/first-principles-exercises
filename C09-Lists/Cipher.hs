module Cipher where

import Data.Char

shiftChar :: Int -> Char -> Char
shiftChar n x 
  | isAsciiLetter x = chr $ start + mod (n + ord x - start) nletters
  | otherwise = x
  where
    isAsciiLetter x = isAscii x && isLetter x
    start
      | isLower x = ord 'a'
      | isUpper x = ord 'A'
      | otherwise = 0
    nletters = 1 + ord 'Z' - ord 'A'

caesar :: Int -> String -> String
caesar n = map $ shiftChar n

unCaesar :: Int -> String -> String
unCaesar n = caesar (-n)

