module FoldBool where

--foldBool :: a -> a -> Bool -> a
--foldBool = error "Error: Need to implement foldBool!"

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y t | t = x
                | otherwise = y
          
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y t = if t then x else y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ True  = x
foldBool3 _ y False = y

