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

-- map function
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

-- concat function
concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

-- concatMap function
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat . map f

{- "and" functions for a list of boolean values
returning True if all the list elements are True (an empty list is True too)
-}
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

{-
similarly define "or" function that returns True if a single element is True
(an empty list is False here)
-}
or' :: [Bool] -> Bool
or' []     = False
or' (x:xs) = x || or' xs

{-
"all" function that returns True if all the results of a predicat function
with all the list elements are True
-}
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = False
all' p xs = and' . map p $ xs

{-
"any" function tests a list if one of the results of the predicat is True
-}
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p xs = or' . map p $ xs

-- Some functions generating endless lists

-- produce of endless sequence of the same value
repeat' :: a -> [a]
repeat' x = xs where xs = x : xs

-- replicate is a function producing a value in a list n times
replicate' :: Int -> a -> [a]
replicate' n x = take n $ repeat' x

-- the cycle function produce an endless sequence repeating its list argument
cycle' :: [a] -> [a]
cycle' [] = error "a bad argument"
cycle' xs = ys where ys = xs ++ ys

{- the iterate function gives a function and an initial value and
produce an endless list like: x -> f x -> f (f x) -> ... -> f.....f(x)
λ: take 5 $ iterate' (^ 2) 3
>> [3,9,81,6561,43046721]
-}
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)


