module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n =
    case n of
         0 -> "Zero"
         1 -> "One"
         2 -> "Two"
         3 -> "Three"
         4 -> "Four"
         5 -> "Five"
         6 -> "Six"
         7 -> "Seven"
         8 -> "Eight"
         9 -> "Nine"
         _ -> "-"

digits :: Int -> [Int]
digits n
    | n < 0 = digits (-n)
    | n < 10 = [n]
    | otherwise = digits div ++ [mod]
    where (div, mod) = divMod n 10


wordNumber :: Int -> String
wordNumber n = intercalate "-" $ map digitToWord $ digits n

