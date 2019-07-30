module Main where

import           Shapes

main :: IO ()
main = print $ nudge (baseCircle 30) 10 20

