import           System.IO
import           Data.Char

main = do
    contents <- readFile "haiku.txt"
    writeFile "haikucaps.txt" $ map toUpper contents
