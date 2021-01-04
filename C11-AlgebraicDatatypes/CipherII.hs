import Data.Char

keyword = "ALLY"
message = "MEET AT DAWN"

isAsciiLetter :: Char -> Bool
isAsciiLetter x = isAscii x && isLetter x

letterNum :: Char -> Int
letterNum x
  | isAsciiLetter x = ord (toLower x) - ord 'a'
  | otherwise = 0


shiftChar :: Int -> Char -> Char
shiftChar n x
  | isAsciiLetter x = chr $ start + mod (n + letterNum x) nletters
  | otherwise = x
  where
    start = ord x - letterNum x
    nletters = 1 + ord 'Z' - ord 'A' -- 26

cipherMessage :: String -> String -> String
cipherMessage ms ks = go ms (concat $ repeat ks)
  where 
    go [] _ = []
    go (' ':ms) ks = ' ' : go ms ks
    go (m:ms) (k:ks) = shiftChar (letterNum k) m : go ms ks

deCipherMessage :: String -> String -> String
deCipherMessage ms ks = go ms (concat $ repeat ks)
  where 
    go [] _ = []
    go (' ':ms) ks = ' ' : go ms ks
    go (m:ms) (k:ks) = shiftChar (negate $ letterNum k) m : go ms ks
