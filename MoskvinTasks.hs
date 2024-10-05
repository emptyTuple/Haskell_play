module MoskvinTasks where

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
sum'n'count = undefined

