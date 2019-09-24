import           System.IO

main = do
    -- type FilePath = String -- 型シノニム
    -- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode -- 列挙型
    handle   <- openFile "haiku.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
