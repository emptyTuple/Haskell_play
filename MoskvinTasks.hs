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
