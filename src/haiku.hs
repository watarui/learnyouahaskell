import           System.IO

main = withFile "haiku.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStr contents
