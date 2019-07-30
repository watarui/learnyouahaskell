module Shapes
    (
    --   Point(..) -- (..) と書くことですべての値コンストラクタがエクスポートされる
    -- , Shape(..)
    -- 値コンストラクタをエクスポートしない
    -- このようなやり方を抽象データ型という
      Point
    , Shape
    , area
    , nudge
    , baseCircle
    , baseRect
    )
where

-- 7 型や型クラス

{-
data Bool = False | True
-- Shape が型、Circle が値コンストラクタ
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
            deriving (Show) -- Shape型をShow型クラスの一員にする

area :: Shape -> Float
area (Circle _ _ r         ) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-}

{-
> area $ Circle 10 20 10
314.15927
-}

-- 7.2.1 Pointデータ型で形を整える

-- 二次元空間の点を表す中間データ構造を作る
-- 点を定義するとき、データ型と値コンストラクタに同じ名前を使う
-- 値コンストラクタがひとつしかないデータ型ではそうするのが慣例

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) =
    (abs $ x2 - x1) * (abs $ y2 - y1)

-- 図形を動かす関数

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
    Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

-- 座標系の原点に円、長方形を作り、移動させる

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point 0 0) (Point w h)

{-
> area (Rectangle (Point 0 0) (Point 100 100))
10000.0
> area (Circle (Point 0 0) 24)
1809.5574
> nudge (Circle (Point 34 34) 10) 5 10
Circle (Point 39.0 44.0) 10.0
> nudge (baseRect 40 100) 60 23
Rectangle (Point 60.0 23.0) (Point 100.0 123.0)
-}
