module Standard where

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p xs = myOr $ fmap p xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (x ==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a-> Bool) -> [a] -> [a]
myFilter p = foldr ((++) . auxMyFilter) []
  where auxMyFilter x = [x | p x]

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr auxMyMaximumBy x xs
  where auxMyMaximumBy x y = if f x y == GT then x else y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy f'
  where f' x y
          | f x y == LT = GT
          | f x y == GT = LT
          | otherwise = EQ

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare

