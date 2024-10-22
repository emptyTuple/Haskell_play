{-# OPTIONS_GHC -Wall #-}  -- Эта прагма добавляет подробности в сообщения об ошибках

module Sandbox where

-- import Data.Char

charToInt :: Char -> Int
charToInt x = read [x]

data Point = Point Double Double deriving Show

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ** 2 + y ** 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = 
  distanceToOrigin $ Point (x1 - x2) (y1 - y2)


