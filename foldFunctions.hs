module FoldFunctions where

import Data.List

-- Правая свёртка ---------------------------------------------------------

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc []      = acc
foldr' f acc (x:xs)  = x `f` foldr' f acc xs

-- Правая свёртка реализует часто повторяющуюся схему рекурсии
-- при операциях над списками, например, рассмотрим рекурсивные реализации
-- для суммы списков и конкатенации списков, а затем реализуем их с foldr

sumList :: Num a => [a] -> a
sumList []      = 0
sumList (x:xs)  = x + sumList xs

sumList' :: Num a => [a] -> a
sumList' = foldr' (+) 0

concatList :: [[a]] -> [a]
concatList []        = []
concatList (xs:yss)  = xs ++ concatList yss

concatList' :: [[a]] -> [a]
concatList' = foldr' (++) []

{-
Можно проиллюстрировать последовательность редукций при правой свёртке
на следующем примере. Следует учесть, что показанная ниже последовательность
рекупсивных вызовов не совсем корректна, так как головным редексом
является `f` в котором также могут быть вычисления. Но будем считать, что
функция f неопределена:

foldr f acc 1:2:3:[]
>> 1 `f` (foldr f acc 2:3:[])
>> 1 `f` (2 `f` (foldr f acc 3:[]))
>> 1 `f` (2 `f` (3 `f` (foldr f acc [])))
>> 1 `f` (2 `f` (3 `f` acc))

То есть список как бы "пересобирается" справа, начиная со значения acc,
но вместо конструктора (:) подставляется функция f
-}

-- Левая свёртка и ее сравнение с правой -------------------------------------

{-
В отличие от правой, в левой свёртке инициализирущее значение помещается 
как бы в начало списка и далее каждый элемент списка посредством функции
связывается с новым значением инициализатора слева направо, то есть, 
используя пример выше:
foldl f acc 1:2:3:[]
>> (((foldl acc []) `f` 1) `f` 2) `f` 3  то есть:
>> ((acc `f` 1) `f` 2) `f` 3
Или в функциональном стиле:
f (f (f acc 1) 2) 3

Отличие типа левой свертки, соотвественно, в типе сворачивающей функции - 
элементы списка в ней находятся на втором месте, а инициализириющее значение
на первом.

Реализация левой свёртки, в отличие от правой, рекурсивный вызов просходит
сразу же на хвосте списка, реализуется хвостовая рекурсия.

* Использовано название foldl'', так как функция foldl' есть в импортированном
пакете Data.List
-}
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' _ acc []      = acc
foldl'' f acc (x:xs)  = foldl'' f (f acc x) xs

{-
Распишем список рекурсивных вызовов на аналогичном предыдущему примере:
 foldl f acc 1:2:3:[]
 >> foldl f (f acc 1) 2:3:[]
 >> foldl f (f (f acc 1) 2) 3:[]
 >> foldl f (f (f (f acc 1) 2) 3) []
 и на пустом списке возвращаем acc:
 >> f (f (f (f acc 1) 2) 3)

Из реализации функции видна ее неэффективность - в результате
рекурсивных вызовов на большом списке в acc накапливается столь же большое
отложенное вычисление (thunk). В Data.List есть более строгая реализация -
foldl', которой и рекомендуется пользоваться
-}

{-
Рассмотрим строгую версию левой свёртки, реализованную в пакете Data.List
под названием foldr'.
Так как отложенные вычисления накапливаются в acc, необходимо форсировать
эти вычисления до WHNF с помощью функции seq, для чего перепишем предыдущию реализацию,
используя where и форсируем вычисление acc
-}
foldl''' :: (b -> a -> b) -> b -> [a] -> b
foldl''' _ acc []      = acc
foldl''' f acc (x:xs)  = acc' `seq` foldl''' f acc' xs
  where acc' = f acc x

{-
Разницу в работе левой и правой свёртки можно также
проиллюстрировать на неассоциативной функции, например (-)

foldr (-) 1 [1,2,3] --> 1 - (2 - (3 - 1)) = 1
foldl (-) 1 [1,2,3] --> ((1 - 1) - 2) - 3 = -5
-}

-- Поведение правой и левой свёртки на беесконечных списках -----------------------

{-
Обратившись к определениям foldl и foldr можно отметить, что при работе с
бесконечными списками, первое уравнение обеих функций никогда не будет работать -
список никогда не будет пустым.
Второе уравнение левой свёртки содержит рекурсивный вызов самой себя и при работе
с бесконечным списком, такой вызов будет происходить постоянно и функция
будет расходящейся, результат не может быть достигнут.
В правой же свёртке в рекуривных вызовах работает функция f - она является
головным редексом. Функция f имеет два аргумента,
второй из которых снова рекурсивный вызов. И в случаях, когда функция f
оказывается нестрогой по второму аргументу может быть получен результат и на
бесконечном списке.

Для иллюстрации используем функцию логического "или" (||) и функцию any,
определённую для конечного списка и возращающую True если в списке есть значение,
возвращающее True для предиката p, и False в противном случае.

* Используем (||| для независимого определения "или")
-}
(|||) :: Bool -> Bool -> Bool
False ||| x  = x
True  ||| _  = True

-- Реализуем функцию any с помощью foldr', определённой выше
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr' (\x acc -> p x ||| acc) False
{- | Данная реализация функции any способна работать с бесконечными списками.
     Её логичным прведением будет работа со списком (в том числе бесконечным)
     до тех пор пока не будет выполнено условие предиката - и тогда она вернет True.
     В случае если в бесконечном списке не будет значений соотвествующих предикату,
     она конечно же разойдется.

     Для понимания такого поведения нужно взглянуть на реализацию лямбды и определение
     для "или", данное выше - в случае, когда предикат даст True (p x), логическое 
     "или" проигнорирует второй аргумент (см. второе уравнение в определении (|||))
     и будет получен результат True, а вычисление списка прекратится.
     any' (> 1000) [1..]
     >> True
-}
