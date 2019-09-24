import           Data.Char

main = do
    contents <- getContents -- contentsはプロミスとしてメモリ上に置かれる
    putStrLn $ map toUpper contents
