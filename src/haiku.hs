import           System.IO
import           Control.Exception

main = withFile' "haiku.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStr contents


-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
-- bracket の第二引数は例外が投げられても呼ばれる
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = bracket (openFile name mode)
                                (\handle -> hClose handle)
                                (\handle -> f handle)
