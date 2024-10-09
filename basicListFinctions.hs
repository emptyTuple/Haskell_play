module BasicListFinctions where

-- sum of list elements
sum' :: Num a => [a] -> a
sum' = helper 0
  where
    helper acc []     = acc
    helper acc (x:xs) = helper (acc + x) xs

-- product of a list
product' :: Num a => [a] -> a
product' [] = 0
product' zs = helper 1 zs
  where
    helper acc []     = acc
    helper acc (x:xs) = helper (acc * x) xs

-- max element of a list
max' :: Ord a => [a] -> a
max' [] = undefined
max' (x:xs) = helper x xs
  where
    helper acc [] = acc
    helper acc (y:ys)
      | acc < y   = helper y ys
      | otherwise = helper acc ys

-- list index operator (!!)
-- index starts from 0
(!!!) :: (Ord a, Integral b) => [a] -> b -> a
_     !!! n | n < 0 = error "index must be positive"
[]     !!! _ = error "index too large"
(x:_)  !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)

