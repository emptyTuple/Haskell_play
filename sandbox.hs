module Sandbox where


newtype Odd = Odd Integer 
  deriving (Eq, Show)

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | even m = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

instance Enum Odd where
    fromEnum (Odd x) = fromInteger x
    toEnum = Odd . toInteger
    succ (Odd x) = Odd (x + 2) 
    pred (Odd x) = Odd (x - 2)
    enumFrom z = z : enumFrom (succ z)
    enumFromTo (Odd a) (Odd b) = map Odd [a, a + 2..b]
    enumFromThen (Odd a) (Odd b) = map Odd [a,b..]
    enumFromThenTo (Odd a) (Odd b) (Odd c) = map Odd [a, b..c]

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change n | n == 0    = [[]]
         | otherwise = [ x : xs | x <- coins, n - x >= 0, xs <- change (n - x) ]


