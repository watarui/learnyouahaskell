{-
stack exec -- ghc helloworld.hs
./helloworld
-}
main = putStrLn "hello, world"
{-
> :t putStrLn
putStrLn :: String -> IO ()
文字列を受け取り、空のタプル（(), unit型）を結果とするI/Oアクションを返す
意味のある返り値がないので、ダミーとして()を返している
-}
