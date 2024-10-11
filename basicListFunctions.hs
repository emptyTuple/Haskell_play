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

-- zip function
zip' :: [a] -> [b] -> [(a,b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- unzip function
unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x, y):xys) = 
  (x:xs, y:ys) 
  where
    (xs, ys) = unzip' xys

unzip'' :: [(a,b)] -> ([a], [b])
unzip'' [] = ([], [])
unzip'' ((x, y):xys) = 
  let (xs, ys) = unzip'' xys
  in (x:xs, y:ys)

-- filter function
filter' :: (t -> Bool) -> [t] -> [t]
filter' _ []     = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

-- takeWhile function
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

-- dropWhile function
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p xs@(x:xs')
  | p x = dropWhile' p xs'
  | otherwise = xs

-- span function
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p xs = (takeWhile' p xs, dropWhile' p xs)

-- break function
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p = span' (not . p)

