module Bintree where

import Peano

-- task t1 
{-
Напишите тип, описывающий бинарные деревья BinTree a. 
Бинарное дерево может быть либо листом со значением типа a, либо хранить два поддерева.
-}
data BTree a = Leaf a | Branch (BTree a) (BTree a)
  deriving (Eq, Show)

-- task t2
{-
Напишите функцию reverse :: BinTree a -> BinTree a, которая переворачивает дерево. 
Она меняет местами два элемента в узле дерева.
-}
reverse' :: BTree a -> BTree a
reverse' ( Leaf x )     = Leaf x
reverse' ( Branch a b ) = Branch (reverse' b) (reverse' a)

-- task t3
{-
Напишите функцию depth :: BinTree a -> Nat, которая вычисляет глубину дерева, 
то есть самый длинный путь от корня дерева к листу.
-}
depth' :: BTree a -> Peano.Nat
depth' ( Leaf _ )     = Peano.Succ Peano.Zero
depth' ( Branch a b ) = Peano.Succ Peano.Zero + max (depth' a) (depth' b)

-- task t4
{-
Напишите функцию leaves :: BinTree a -> [a], которая переводит бинарное дерево в список, 
возвращая все элементы в листьях дерева.
-}
leaves :: BTree a -> [a]
leaves ( Leaf a )     = [a]
leaves ( Branch a b ) = leaves a ++ leaves b 



