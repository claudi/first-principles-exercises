isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee y _ Nothing = y
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap go
  where
    go Nothing = []
    go (Just x) = [x]

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = if elemNothing ms then Just $ catMaybes ms else Nothing
  where elemNothing ms = all go ms
        go Nothing = False
        go (Just _) = True
