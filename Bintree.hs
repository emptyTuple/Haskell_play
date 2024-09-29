module Bintree where

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
reverse :: BTree a -> BTree a
reverse = undefined

