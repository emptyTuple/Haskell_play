module Peano where

-- successor (succedent)
data Nat = Nil | Succ Nat
  deriving (Show, Ord)

instance Eq Nat where
  (==) a b =
    case (a, b) of
      (Nil, Nil)         -> True
      (Succ a', Succ b') -> a' == b'
      _                  -> False

instance Num Nat where
  (+) Nil a        = a
  (+) a Nil        = a
  (+) a (Succ b)   = Succ (a + b)
  --(+) (Succ a) b = Succ (a + b) not efficient
  (*) Nil _        = Nil
  (*) _ Nil        = Nil
  (*) a (Succ b)   = a + (a * b)
  fromInteger 0    = Nil
  fromInteger n    = Succ (fromInteger (n - 1))
  abs x            = x
  negate           = error "Undefined"
  signum Nil       = Nil
  signum _         = Succ Nil
  
-- neighbours
isNeighbour :: Nat -> Nat -> Bool
isNeighbour a b
  | a == Succ b = True
  | b == Succ a = True
  | otherwise   = False

-- not neighbour
isNotNeighbour :: Nat -> Nat -> Bool
isNotNeighbour a b 
  | a == b      = False
  | a == Succ b = False
  | b == Succ a = False
  |otherwise    = True

-- | get both neighbours
getBothNeighbours :: Nat -> (Nat, Nat)
getBothNeighbours Nil = error "Undefined"
getBothNeighbours x@(Succ y) = (y, Succ x)

-- pow function
pow :: Nat -> Nat -> Nat
pow _ Nil      = Succ Nil
pow a (Succ b) = a * pow a b

-- | even/odd
isEven :: Nat -> Bool
isEven Nil            = True
isEven (Succ Nil)     = False
isEven x              = isEven y
  where Succ (Succ y) = x

-- get predecessor
pred' :: Nat -> Nat
pred' Nil = error "Undefined"
pred' (Succ y) = y
