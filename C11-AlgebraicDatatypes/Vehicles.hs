data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacterer
    = Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline
    = PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle
    = Car Manufacterer Price
    | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _  = False

getManu :: Vehicle -> Manufacterer
getManu (Car m _) = m
getMano _ = error "No manufacterer found"

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar
