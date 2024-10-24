module MoskvinTasks where

import Data.Char
import Data.List (unfoldr)

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

{-
Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие 
из символов в верхнем регистре. Предполагается, что текст состоит только из символов 
алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.
GHCi> delAllUpper "Abc IS not ABC"
GHCi> "Abc not"
Постарайтесь реализовать эту функцию как цепочку композиций.
-}
delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words

delAllUpper' :: String -> String
delAllUpper' = unwords . filter (any isLower) . words

{-
Напишите функцию max3, которой передаются три списка одинаковой длины и которая 
возвращает список той же длины, содержащий на k-ой позиции наибольшее значение 
из величин на этой позиции в списках-аргументах.
GHCi> max3 [7,2,9] [3,6,8] [1,8,10]
GHCi> [7,8,10]
GHCi> max3 "AXZ" "YDW" "MLK"
GHCi> "YXZ"
-}
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y -> max (max x y))

{-
Реализуйте c использованием функции zipWith функцию fibStream, возвращающую 
бесконечный список чисел Фибоначчи.
-}
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (drop 1 fibStream)

{-
Реализуйте функцию meanList, которая находит среднее значение элементов списка, 
используя однократный вызов функции свертки.
GHCi> meanList [1,2,3,4]
>> 2.5
Постобработка считается допустимой, то есть предполагаемая реализация 
функции meanList имеет вид:
meanList = someFun . foldr someFoldingFun someIni
-}
meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (s, l) -> (x + s, l + 1)) (0, 0)

{-
Используя однократный вызов свертки, реализуйте функцию evenOnly, которая выбрасывает 
из списка элементы, стоящие на нечетных местах, оставляя только чётные.
GHCi> evenOnly [1..10]
>> [2,4,6,8,10]
GHCi> evenOnly ['a'..'z']
>> "bdfhjlnprtvxz"
-}
evenOnly :: [a] -> [a]
evenOnly = undefined

{-
Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке
список символов, попадающих в заданный парой диапазон. Попадание символа x в диапазон 
пары (a,b) означает, что x >= a и x <= b.
GHCi> revRange ('a','z')
>> "zyxwvutsrqponmlkjihgfedcba"
-}
revRange :: (Char,Char) -> [Char]
revRange = unfoldr (\(a, b) -> if pred a >= b then Nothing else Just (b, (a, pred b)))

