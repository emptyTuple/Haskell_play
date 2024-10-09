module BasicListFunctions where

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

-- list index operator like (!!)
-- index starts from 0
(!!!) :: Integral b => [a] -> b -> a
_      !!! n | n < 0 = error "Index must be positive."
[]     !!! _         = error "Index out of range."
(x:_)  !!! 0         = x
(_:xs) !!! n         = xs !!! (n - 1)

-- take function
take' :: Integral a => a -> [b] -> [b]
take' n _ | n <= 0 = []
take' _ []         = []
take' n (x:xs)     = x : take' (n - 1) xs

-- drop function
drop' :: Integral a => a -> [b] -> [b]
drop' n xs | n <= 0 = xs
drop' _ []          = []
drop' n (_:xs)      = drop' (n - 1) xs

-- splitAt function
splitAt' :: Integral a => a -> [b] -> ([b], [b])
splitAt' n xs = (take' n xs, drop' n xs)
