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
depth' :: BTree a -> Nat
depth' ( Leaf _ )     = Succ Zero
depth' ( Branch a b ) = Succ Zero + max (depth' a) (depth' b)

