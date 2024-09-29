{-# LANGUAGE MultiWayIf #-}  -- подключение расширения

-- БАЗОВЫЕ ЛОГИЧЕСКИЕ КОНСТРУКЦИИ

-- многоуровневая конструкция if then else
-- компилятор ругается и предлагает использовать quards, поэтому закомментим (но код работает)
{--
checkGold :: Int -> String
checkGold probe =
    if probe == 999
        then "standart 999"
        else if probe == 750
            then "standart 750"
            else if probe == 585
                then "standart 585"
                else "not gold"
--}

-- конструкция MultiIf, требует подключения расширения MultiIf (строка 1)
-- есть знак равенства в определении функции, поэтому используются стрелки после логических выражений

checkGold' :: Int -> String
checkGold' probe =
    if | probe == 999 -> "standart 999"
       | probe == 750 -> "standart 750"
       | probe == 585 -> "standart 585"
       | otherwise -> "not gold"

-- конструкция с guards. Нет знака равенства в определении функции, знаки равенства используются
-- в каждом guard после логических выражений

checkGold'' :: Int -> String
checkGold'' probe
    | probe == 999 = "standart 999"
    | probe == 750 = "standart 750"
    | probe == 585 = "standart 585"
    | otherwise = "not gold"

-- Сравение с образцом (pattern matching). Перечисляем определения функции одно за одним.

checkGold''' :: Int -> String
checkGold''' 999 = "standart 999"
checkGold''' 750 = "standart 750"
checkGold''' 585 = "standart 585"
checkGold''' _ = "not gold"

-- pattern matching с помощью конструкции `case ... of`. Есть знак равенства в определении функции, 
-- поэтому используем стрелки

checkGold'''' :: Int -> String
checkGold'''' probe =
    case probe of
        999 -> "standart 999"
        750 -> "standart 750"
        585 -> "standart 585"
        _   -> "not gold"

-- ВЫРАЖЕНИЕ let <DECLARATIONS> in <EXPRESSION> позволяет ввести поясняющие выражения,
-- которые упрощяют понимание используемых в коде значений
-- После in должно быть выражение, поэтому вариант с guards не будет работать!

-- вариант с мультииф, но hlint ругается и предлагает заменить его на обычный if else
calculateTime :: Int -> Int
calculateTime t =
    let threshhold = 50
        correction = 100
        delta = 10
    in
        if | t < threshhold -> t  + correction
           | otherwise -> t + correction + delta

-- Также можно сократить область видимости введённых промежуточных выражений, например
-- опустим выражение delta на уровень ниже, чтобы оно было видно только на уровне otherwise
-- а также используем конструкцию if else вместо мультииф, как и рекомендует линтер
calculateTime' :: Int -> Int
calculateTime' t = 
    let threshhold = 50
        correction = 100
    in
        if t < threshhold 
            then t + correction
            else
                let delta = 10
                in
                    t + correction + delta

-- Также промежуточные выражения можно вводить в одну строку через точку с запятой:
-- let threshold = 40; correction = 120

-- ВЫРАЖЕНИЕ WHERE
-- Ключевое слово where делает примерно то же, что и let, 
-- но промежуточные выражения задаются в конце функции. В отличие от let все введённые выражения
-- видимы в любой части выражения, предшествующего where

calculateTime'' :: Int -> Int
calculateTime'' t = 
    if t < threshold
        then t + correction
        else t + correction + delta
    where
        threshold = 50
        correction = 100
        delta = 10

-- также можно комбинировать where и let ... in но не стоит этого делать без необходимости
-- Можно использовать выражения объявленные в where в выражении let .. in, но наоборот не работает

