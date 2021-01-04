import Data.Char


type Presses = Int
data Digit = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    | Star | Zero | Pound    deriving (Show)

data Key = Key Digit [Char] deriving (Show)
data Phone = Phone [Key] deriving (Show)

phone = Phone
    [ Key One "1"
    , Key Two "ABC2"
    , Key Three "DEF3"
    , Key Four "GHI4"
    , Key Five "JKL5"
    , Key Six "MNO6"
    , Key Seven "PQRS7"
    , Key Eight "TUV8"
    , Key Nine "WXYZ9"
    , Key Star "^*"
    , Key Zero "+ 0"
    , Key Pound ".,#"
    ]

convo :: [String]
convo =
    [ "Wanna play 20 questions"
    , "Ya"
    , "U 1st haha"
    , "Lol ok. Have u ever tasted alcohol lol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "Ok. Do u think I am pretty Lol"
    , "Lol ya"
    , "Haha thanks just making sure rofl ur turn"
    ]

--getDigit :: Key -> (Digit, Presses)
--getDigit (Key d _) = d

getKeys :: Phone -> [Key]
getKeys (Phone ks) = ks

searchKey :: [Key] -> Char -> (Digit, Presses)
searchKey ks c = getDigit $ find (not . elemKey) ks
  where
    getDigit (Key d cs) = (d, snd $ find (\(x, _) -> x /= c') (zip cs [1..]))
    find f xs = head $ dropWhile f xs
    c' = toUpper c
    elemKey (Key _ xs) = c' `elem` xs

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps ph@(Phone ks) c
  | isUpper c = reverseTaps ph '^' ++ reverseTaps ph (toLower c)
  | otherwise = [searchKey ks c]

cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead ph = concatMap (reverseTaps ph)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . fmap snd

mostPopularLetter :: String -> Char
mostPopularLetter = fst . head . sortOn f . countLetters . map toUpper . filter isLetter
  where f (_, n) = n

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn f (x:xs) = sortOn f [ y | y <- xs, f y > f x ] ++ [x] ++ sortOn f [ y | y <- xs, f y <= f x ]

count :: Eq a => a -> [a] -> (a, Int)
count x xs =  (x, length $ filter (x ==) xs)

countLetters :: String -> [(Char, Int)]
countLetters xs = fmap (($ xs) . count) ['A' .. 'Z']

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

