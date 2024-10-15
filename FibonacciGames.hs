module FibonacciGames where

-- naive implementation for positive, O(n^2)
fibonacci' :: Int -> Integer
fibonacci' n | n == 0    = 0
             | n == 1     = 1
             | n > 0      = fibonacci' (n - 1) + fibonacci' (n - 2)
             | otherwise  = undefined 

{- with two arguments helper function, O(n),
   for both negative and positive
-}
fibonacci'' :: Int -> Integer
fibonacci'' = helper 0 1
  where
    helper a b x
      | x == 0     = a
      | x > 0      = helper b (a + b) (x - 1)
      | otherwise  = helper (b - a) a (x + 1)

-- with tuple accumulator helper function, O(n)
fibonacci''' :: Int -> Integer
fibonacci''' = helper (0,1)
  where helper (x, _) 0 = x
        helper (x, y) n = helper (y, x + y) (n - 1)

{-  | take n-th number from endless fibonacci list,
      count from 1: fib 1 -> 0, fib 2 -> 1, fib 3 -> 1 ... fib 20 -> 4181 ... 
-}
getNthFib :: Int -> Integer
getNthFib n
  | n <= 0    = error "start from 1"
  | otherwise = endlessFib !! (n - 1)
      where endlessFib = 0 : 1 : zipWith (+) endlessFib (drop 1 endlessFib)

