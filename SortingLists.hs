module SortingLists where

-- | INSERT SORT
insertSort :: Ord t => [t] -> [t]
insertSort [] = []
insertSort (x:xs) = insert x $ insertSort xs
  where
    insert y [] = [y]
    insert y (z:zs)
      | y < z = y : z : zs
      | otherwise = z : insert y zs

