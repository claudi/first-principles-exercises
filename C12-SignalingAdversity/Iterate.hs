module Iterate where

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ (myIterate f (f x))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f y = case f y of
                     Nothing -> []
                     Just (x, y') -> [x] ++ myUnfoldr f y'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr f' x
  where f' x = Just (x, f x)

