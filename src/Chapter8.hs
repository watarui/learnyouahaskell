-- module Chapter8 where

import           Control.Monad
import           Data.Char

{-
main :: IO () -- 型宣言
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hey " ++ name ++ ", you rock!" -- doブロックの最後の行は束縛（<-）を行わない
-}

{-
import           Data.Char
main :: IO ()
main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    -- <- はI/Oアクションを実行してその結果に名前を束縛する
    -- letは純粋な式に名前を束縛する
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
-}

putStr' :: String -> IO ()
putStr' []       = return ()
putStr' (x : xs) = do
    putChar x
    putStr' xs

{-
main :: IO ()
main = do
    input <- getLine
    when (input == "SWORDFISH") $ putStrLn input
-}

{-
main :: IO ()
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
-}

{-
> mapM print [1,2,3]
1
2
3
[(),(),()]
-- I/Oアクションの結果を捨てる
> mapM_ print [1,2,3]
1
2
3
-}

{-
main :: IO ()
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
-}

{-
main :: IO ()
main = forever $ do
    putStr "Input: "
    l <- getLine
    putStrLn $ map toUpper l
-}

main :: IO [()]
main = do
    colors <- forM [1, 2, 3, 4] $ \a -> do
        putStrLn $ "Which color " ++ show a ++ "?"
        getLine
    putStrLn "[1, 2, 3, 4]: "
    mapM putStrLn colors
