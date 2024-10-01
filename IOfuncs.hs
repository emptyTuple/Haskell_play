module IOfuncs where

printFromTo :: Int -> Int -> IO ()
printFromTo x y
  | x > y = error "undefined"
  | x == y = print y
  | otherwise = do print x
                   printFromTo (x + 1) y


