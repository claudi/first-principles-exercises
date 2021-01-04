lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where
    go (Left x) xs = x : xs
    go (Right _) xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where
    go (Right x) xs = x : xs
    go (Left _) xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right y) = g y

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
