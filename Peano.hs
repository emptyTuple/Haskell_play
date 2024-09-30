module Peano where

-- succedent
data Nat = Zero | Succ Nat
  deriving (Show, Ord)

instance Eq Nat where
  (==) a b =
    case (a, b) of
      (Zero, Zero)       -> True
      (Succ a', Succ b') -> a' == b'
      _                  -> False

instance Num Nat where
  (+) Zero a = a
  (+) a Zero = a
  (+) a (Succ b) = Succ (a + b)
  --(+) (Succ a) b = Succ (a + b) not effective!
  negate = error "Undefined"
  (*) Zero _ = Zero
  (*) _ Zero = Zero
  (*) a (Succ b) = a + (a * b)
  --(*) (Succ a) b = b + (a * b)
  abs x = x
  signum Zero = Zero
  signum _  = Succ Zero
  fromInteger 0 = Zero
  fromInteger n = Succ (fromInteger (n - 1))
  
-- neighbours
isNeighbour :: Nat -> Nat -> Bool
isNeighbour a b
  | a == Succ b = True
  | b == Succ a = True
  | otherwise = False

-- not neighbour
isNotNeighbour :: Nat -> Nat -> Bool
isNotNeighbour a b 
  | a == b = False
  | a == Succ b = False
  | b == Succ a = False
  |otherwise = True

-- pow function
pow :: Nat -> Nat -> Nat
pow _ Zero = Succ Zero
pow a (Succ b) = a * pow a b
