import           System.IO
import           System.Directory
import           Data.List
import           Control.Exception

main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks =
            zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
    putStrLn "There are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number       = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks -- !! は添字に対応するリストの要素を返す
    -- bracketでは処理が終わると常に獲得したリソースを開放するのに対し、
    -- bracketOnErrorは何らかの例外が発生したときのみリソースを開放する
    bracketOnError
        (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do -- エラーが発生したときの処理
            hClose tempHandle
            removeFile tempName
        )
        (\(tempName, tempHandle) -> do -- 一時ファイルを使ってやりたい処理
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt"
        )
