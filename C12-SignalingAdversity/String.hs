import Data.Char

isVowel :: Char -> Bool
isVowel c
  | c `elem` "aeiouAEIOU" = True
  | otherwise = False

notThe :: String -> Maybe String
notThe w 
  | w == "the" = Nothing
  | otherwise = Just w

replaceThe :: String -> String
replaceThe = unwords . fmap (the' . notThe) . words
  where
    the' Nothing = "a"
    the' (Just w) = w

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go [] = 0
    go [_] = 0
    go ("the":xs) = go xs + if isVowel (head $ head xs) then 1 else 0
    go (x:xs) = 0 + go xs

countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w = if countVowels w > toInteger (length w)  `div` 2 then Nothing else Just (Word' w)
