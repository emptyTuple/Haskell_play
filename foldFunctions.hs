module FoldFunctions where

import Data.List

-- Правая свёртка ---------------------------------------------------------
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc []      = acc
foldr' f acc (x:xs)  = x `f` foldr' f acc xs

-- Правая свёртка по сути реализует часто повторяющуюся схему рекурсии
-- при операциях над списками, например, рассмотрим рекурсивные реализации
-- для суммы списков и конкатенации списков, а затем реализуем их с foldr

sumList :: Num a => [a] -> a
sumList []      = 0
sumList (x:xs)  = x + sumList xs

sumList' :: Num a => [a] -> a
sumList' = foldr' (+) 0

concatList :: [[a]] -> [a]
concatList [] = []
concatList (xs:yss) = xs ++ concatList yss

concatList' :: [[a]] -> [a]
concatList' = foldr' (++) []
-- ------------------------------------------------------------------------

