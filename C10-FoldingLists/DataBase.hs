import Data.Time
import Data.Maybe (mapMaybe)

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

getString :: DatabaseItem -> Maybe String
getString (DbString s) = Just s
getString _ = Nothing

getNumber :: DatabaseItem -> Maybe Integer
getNumber (DbNumber n) = Just n
getNumber _ = Nothing

getDate :: DatabaseItem -> Maybe UTCTime
getDate (DbDate t) = Just t
getDate _ = Nothing

filterString'' :: [DatabaseItem] -> [String]
filterString'' = mapMaybe getString

filterNumber'' :: [DatabaseItem] -> [Integer]
filterNumber'' = mapMaybe getNumber

filterDbDate'' :: [DatabaseItem] -> [UTCTime]
filterDbDate'' = mapMaybe getDate

filterDb :: (DatabaseItem -> Maybe a) -> [DatabaseItem] -> [a]
filterDb = mapMaybe

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
               (fromGregorian 1911 5 1)
               (secondsToDiffTime 34123)
             )
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
               (fromGregorian 1921 5 1)
               (secondsToDiffTime 34123)
             )
    ]

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' [] = []
filterDbDate' (x:xs) = case x of
                           (DbDate t) -> t : filterDbDate' xs
                           _ -> filterDbDate' xs

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filterHelper []
  where filterHelper x y =
          case x of
               (DbDate t) -> t : y
               _ -> y

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterHelper []
  where filterHelper x y =
          case x of
               (DbNumber n) -> n : y
               _ -> y

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral $ sumDb xs / fromIntegral $ length $ filterDbNumber xs

