-- оператор - это функция, используемая в инфиксной записи
-- для использования оператора как функции используем круглые скобки
summator :: Int -> Int -> Int -> Int
summator x y z =
    (+) x y + z

-- точно также можно использовать backticks для использования
-- функций в инфиксон стиле: 5 `div` 2

{-
Приоритеты операторов:
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
