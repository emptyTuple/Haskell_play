module MoskvinTasks where

import Data.Char

-- Некоторые задачи с курса Дениса Москвина

{-
Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности:
    a0 = 1, a1 = 2, a2 = 3 ... a(k+1) = a(k+2) + a(k+1) - 2*a(k)
-}
seqA :: Integer -> Integer
seqA n = let
    helper x _ _ 0 = x
    helper x y z a = helper y z (y + z - 2 * x) (a - 1)
         in helper 1 2 3 n

-- Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n = (x1, x2)
  where
    x1 = toInteger . length . show . abs $ n
    x2 = toInteger . sum . map digitToInt . show. abs $ n


sum'n'count' :: Integer -> (Integer, Integer)
sum'n'count' n = helper (abs n) 0 0 
  where
    helper 0 0 0 = (0, 1)
    helper 0 sm am = (sm, am)
    helper x sm am = helper (div x 10) (sm + mod n 10) (am + 1)

{-
Напишите функцию groupElems которая группирует одинаковые элементы в списке 
(если они идут подряд) и возвращает список таких групп.
Разрешается использовать только функции, доступные из библиотеки Prelude.
-}
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x : y : xs) | x /= y = [x] : groupElems (y : xs)
groupElems (x : xs) =
  case groupElems xs of
    x' : xs' -> (x : x') : xs'
    _ -> [[x]]


{-
Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.
-}
readDigits :: String -> (String, String)
readDigits = span isDigit

readDigits' :: String -> (String, String)
readDigits' [] = ([], [])
readDigits' z@(x:xs)
  | isDigit x = 
      let (prefix, body) = readDigits' xs
      in (x:prefix, body)
  | otherwise = ([], z)

{-
Реализуйте функцию filterDisj, принимающую два унарных предиката и список, 
и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.
-}
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs)
  | p1 x || p2 x = x : filterDisj p1 p2 xs
  | otherwise = filterDisj p1 p2 xs

{-
Напишите функцию squares'n'cubes, принимающую список чисел,
и возвращающую список квадратов и кубов элементов исходного списка.
-}

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3]) 

squares'n'cubes' :: Floating a => [a] -> [a]
squares'n'cubes' [] = []
squares'n'cubes' (x:xs) = x ** 2 : x ** 3 : squares'n'cubes' xs

{-
Воспользовавшись функциями map и concatMap, определите функцию perms, 
которая возвращает все перестановки, которые можно получить из данного списка, 
в любом порядке.
Считайте, что все элементы в списке уникальны, и что для пустого списка 
имеется одна перестановка.
GHCi> perms [1,2,3]
GHCi> [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-}
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (addX x) (perms xs)
  where
    addX z [] = [[z]]
    addX z yss@(y:ys) = (z:yss) : map (y:) (addX z ys)

