module Jammin where
 
import Data.List

data Fruit
    = Peach
    | Plum
    | Apple
    | Blackberry
    deriving (Eq, Ord, Show)

data JamJars
    = Jam { getFruit :: Fruit
          , getCount :: Int
          }
    deriving (Eq, Ord, Show)

row1 = Jam Peach 10
row2 = Jam Blackberry 23
row3 = Jam Apple 14
row4 = Jam Plum 12
row5 = Jam Apple 10
row6 = Jam Plum 10
allJam =
    [ row1
    , row2
    , row3
    , row4
    , row5
    , row6
    ]

countJars :: [JamJars] -> Int
countJars = sum . fmap getCount

mostRow :: [JamJars] -> JamJars
mostRow = maximumBy (\j1 j2 -> compare (getCount j1) (getCount j2))

sortJams :: [JamJars] -> [JamJars]
sortJams = sortBy (\j1 j2 -> compare (getFruit j1) (getFruit j2))

groupJams :: [JamJars] -> [[JamJars]]
groupJams = groupBy (\j1 j2 -> getFruit j1 == getFruit j2) . sortJams

--countJams :: [JamJars] -> [Int]
--countJams = fmap countJars . groupJams
