module IOfuncs where

-- | print values from x to y
printFromTo :: Int -> Int -> IO ()
printFromTo x y
  | x > y = error "undefined"
  | x == y = print y
  | otherwise = do print x
                   printFromTo (x + 1) y

-- ask for non empty input
nonEmpty :: IO String
nonEmpty = do
    x <- getLine
    case x of
        "" -> do putStrLn "empty, try again:"
                 nonEmpty
        _  -> return x

