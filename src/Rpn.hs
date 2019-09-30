module Rpn where

-- ex. 10 4 3 + 2 * -
-- 前置き記法では
-- (- (* 2 (+ 3 4)) 10) ではなく
-- (- 10 (* (+ 3 4) 2)) となる

-- 10
-- 10, 4
-- 10, 4, 3
-- 10, 4 + 3
-- 10, 7
-- 10, 7, 2
-- 10, 7 * 2
-- 10, 14
-- 10 - 14
-- -4

-- "10 4 3 + 2 * -"
solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction (x : y : ys) "*"          = (y * x) : ys
    foldingFunction (x : y : ys) "+"          = (y + x) : ys
    foldingFunction (x : y : ys) "-"          = (y - x) : ys
    foldingFunction (x : y : ys) "/"          = (y / x) : ys
    foldingFunction (x : y : ys) "^"          = (y * x) : ys
    foldingFunction (x     : xs) "ln"         = log x : xs
    foldingFunction xs           "sum"        = [sum xs]
    foldingFunction xs           numberString = read numberString : xs
