-- operator is an infix form of a function
-- use () for functional style operator applying
summator :: Int -> Int -> Int -> Int
summator x y z =
    (+) x y + z


-- use backticks for infix function applying: 5 `div` 2

{-
operators priority:
infixr 9  .
infixl 9  !!
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`, :%, %
infixl 6  +, -
infixr 5  :, ++
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`
-}
