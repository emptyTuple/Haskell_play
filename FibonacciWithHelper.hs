module FibonacciWithHelper where

-- naive for positive
fibonacci' :: Integer -> Integer
fibonacci' n | n == 0    = 0
            | n == 1    = 1
            | n > 0     = fibonacci' (n - 1) + fibonacci' (n - 2)
            | otherwise = undefined 

-- with helper, O(n), for all integers
fibonacci :: Integer -> Integer
fibonacci = helper 0 1
  where
    helper a b x
      | x == 0 = a
      | x > 0  = helper b (a + b) (x - 1)
      | otherwise  = helper (b - a) a (x + 1)
      
