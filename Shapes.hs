module Shapes(
 Point(..)
,Shape(..)
,surface
,nudge
,baseCircle
,baseRect
) where

data Point = Point Float Float deriving (Show,Eq,Read)
data Shape = Circle Point Float | Rectangle Point Point
  deriving (Show,Eq,Read)
surface :: Shape -> Float
surface (Circle _ r) = pi * r * r
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: (Shape) -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) ( y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape -- 注意这里是Shape 不是 Circle
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect x y = Rectangle (Point 0 0) (Point x y)
