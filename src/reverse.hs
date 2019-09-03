main = do
    line <- getLine
    if null line
        then return () -- return は純粋な値をI/Oアクションに変換する（終了する働きはない）
        else do
            putStrLn $ reverseWords line
            main -- 入力が null でない限り再帰させる

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
