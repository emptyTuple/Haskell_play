module SortingLists where

-- | INSERT SORT O(n^2)
insertSort :: Ord t => [t] -> [t]
insertSort [] = []
insertSort (x:xs) = insert x $ insertSort xs
  where
    insert y [] = [y]
    insert y (z:zs)
      | y < z = y : z : zs
      | otherwise = z : insert y zs

