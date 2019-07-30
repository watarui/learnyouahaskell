module Chapter6 where
-- 6 モジュール

-- import Data.List ( nub, sort )
import           Data.List
import           Data.Char

-- 修飾付きインポート（qualified）
-- qualified のついたものを明示的にインポートする
-- Data.Map.filter と指定しなければいけないので、asで短くする
-- ex. filter, M.filter


import qualified Data.Map                      as Map

-- import Data.List hiding (nub, sort)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub -- nub xs : リストから重複する要素を取り除く

-- 6.2 標準モジュールを使ってみる

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

-- c.f. Data.List.isInfixOf
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

-- 6.2.3 シーザー暗号

-- 各文字をアルファベット上で一定の数だけシフトする

{-
> :m + Data.Char
> ord 'a'
97
> chr 97
'a'
> map ord "abcdefgh"
[97,98,99,100,101,102,103,104]
-}

encode :: Int -> String -> String
-- encode offset msg = map (\c -> chr $ ord c + offset) msg
encode offset = map $ chr . (+ offset) . ord

decode :: Int -> String -> String
decode shift = encode $ negate shift

-- 6.2.4 正格な左畳み込みについて

-- foldl ではスタックオーバーフローが起こる可能性がある（遅延評価なので）
-- アキュームレータの演算をスタックしておく。
-- Data.List.foldl' は正格

{-
> Data.List.foldl' (+) 0 (replicate 100000000 1)
100000000
-}

-- import Data.Char

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

{-
> :t find
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
Maybe a型の値は、0個か1個の要素だけを持てる
この方は失敗する可能性があることを表現しており、
何も持っていないという値を作るにはNothingを使う

> Nothing
Nothing
> Just "hey"
Just "hey"
> Just 3
Just 3
> :t Just "hey"
Just "hey" :: Maybe [Char]
> :t Just
Just :: a -> Maybe a

find は述部を満たす要素が見つかったら、その要素をJust でラップしたものを返す。
見つからなければ Nothing を返す
-}

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1 ..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

-- 6.3 キーから値へのマッピング

-- 6.3.1 連想リスト（辞書）

phoneBook =
    [ ("betty"  , "555-2938")
    , ("bonnie" , "452-2928")
    , ("patsy"  , "493-2928")
    , ("lucille", "205-2928")
    , ("wendy"  , "939-8282")
    , ("penny"  , "853-2492")
    ]

-- c.f. Data.List.lookup
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key = snd . head . filter (\(k, v) -> key == k) -- head [] でエラーが出る

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k, v) : xs) | key == k  = Just v
                           | otherwise = findKey' key xs

-- 規定部と、リストへのheadとtailへの分割と再帰呼び出しが揃っている
-- -> 畳み込みを使ったほうが良い
findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key xs =
    foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

-- 6.3.2 Data.Map

-- Map.fromList : 連想リストをMapに変換する

phoneBook' :: Map.Map String String
phoneBook' = Map.fromList
    [ ("betty"  , "555-2938")
    , ("bonnie" , "452-2928")
    , ("patsy"  , "493-2928")
    , ("lucille", "205-2928")
    , ("wendy"  , "939-8282")
    , ("penny"  , "853-2492")
    ]

-- lookup キーとMapを受け取り対応する値をMapから探し、成功したら値をJustで包んで返し、失敗したらNothingを返す

{-
> phoneBook'
fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928"),("patsy","493-2928"),("penny","853-2492"),("wendy","939-8282")]
> :t Map.lookup
Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a
> :t Map.lookup "wendy" phoneBook'
Map.lookup "wendy" phoneBook' :: Maybe String
> Map.lookup "wendy" phoneBook'
Just "939-8282"
> :t Map.insert
Map.insert :: Ord k => k -> a -> Map.Map k a -> Map.Map k a
> let newBook = Map.insert "grace" "341-9021" phoneBook'
> Map.lookup "grace" newBook
Just "341-9021"
> :t Map.size
Map.size :: Map.Map k a -> Int
> Map.size phoneBook'
6
> Map.size newBook
7
-}

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

{-
> let intBook = Map.map string2digits phoneBook'
> :t intBook
intBook :: Map.Map String [Int]
> Map.lookup "betty" intBook
Just
-}

-- 重複ありの連想リストの場合
phoneBook2 =
    [ ("betty"  , "555-2938")
    , ("betty"  , "342-2492")
    , ("bonnie" , "452-2928")
    , ("patsy"  , "493-2928")
    , ("patsy"  , "943-2929")
    , ("patsy"  , "827-9162")
    , ("lucille", "205-2928")
    , ("wendy"  , "939-8282")
    , ("penny"  , "853-2492")
    , ("penny"  , "555-2111")
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs where add n1 n2 = n1 ++ ", " ++ n2

{-
> Map.lookup "patsy" $ phoneBookToMap phoneBook2
Just "827-9162, 943-2929, 493-2928"
-}

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
-- phoneBookToMap' xs = Map.fromListWith (++) xs $ map (\(k, v) -> (k, [v])) xs
phoneBookToMap' = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))

{-
> Map.lookup "patsy" $ phoneBookToMap' phoneBook2
Just ["827-9162","943-2929","493-2928"]
-}

-- 6.4 モジュールを作る
