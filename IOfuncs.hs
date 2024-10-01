module IOfuncs where

-- | It is not a function, it is an IO action
ioaction :: IO ()
ioaction = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")

-- | count from x to y
countFromTo :: Int -> Int -> IO ()
countFromTo x y
  | x > y      = do return ()
  | x == y     = do print y
  | otherwise  = do print x
                    countFromTo (x + 1) y

-- | ask for non empty input
nonEmpty :: IO String
nonEmpty = do
    x <- getLine
    case x of
        "" -> do putStrLn "empty, try again:"
                 nonEmpty
        _  -> return x

