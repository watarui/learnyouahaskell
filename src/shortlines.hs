import           Data.Char

-- main = do
    -- contents <- getContents -- contentsはプロミスとしてメモリ上に置かれる
    -- putStr (shortLinesOnly contents)
main = interact shortLinesOnly


shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines
