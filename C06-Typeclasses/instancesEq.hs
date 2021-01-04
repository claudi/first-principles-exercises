module Instances where

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn u) (TisAn u') = u == u'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two u v) (Two u' v') = (u == u') && (v == v')

data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt u) (TisAnInt u') = u == u'
  (==) (TisAString v) (TisAString v') = v == v'
  (==) _           _            = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair u v) (Pair u' v') = (u == u') && (v == v')

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple u v) (Tuple u' v') = (u == u') && (v == v')

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne u) (ThisOne u') = u == u'
  (==) (ThatOne v) (ThatOne v') = v == v'
  (==) _           _            = False

data EitherOr a b =
    Hello a
  | GoodBye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello u) (Hello u') = u == u'
  (==) (GoodBye v) (GoodBye v') = v == v'
  (==) _           _            = False

