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
