module Functions where

removeNonUppercase :: [Char] -> [Char] -- 型シグネチャ
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z'] ]

addTree :: Int -> Int -> Int -> Int
addTree x y z = x + y + z

-- 2.2 一般的な Haskell の型

-- Int : 整数 有界（最大値と最小値がある - 2^63 ... 2^63 - 1）

-- Integer : 整数 有界でない

factorial :: Integer -> Integer
factorial n = product [1 .. n]

-- Float : 単精度浮動小数点数

circumference :: Float -> Float
circumference r = 2 * pi * r

-- Double 倍精度浮動小数点数

circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- Bool : True or False

-- Char : Unicode シングルクォートで囲む

-- Tuple : 定義は要素の型と数に依る。 () はユニット

-- 2.3 型変数

{-
> :t head
head :: [a] -> a -- 任意の型のリストを引数とし、その型の要素を1つ返す
-}

-- 型変数を用いた関数は多層的関数と呼ばれる
{-
> :t fst
fst :: (a, b) -> a -- 任意の型のペアの1つ目の要素を返す
-}

-- 2.4 型クラス

-- 振る舞いを定義するインターフェイス
-- 型クラスのインスタンスが型
-- 型クラスは関数の定義の集まり
-- 型クラスに属する関数をメソッドと呼ぶ

{-
> :t (==)
(==) :: Eq a => a -> a -> Bool
-}

-- => : 型クラス制約
-- 等値性関数は、同じ型の任意の2つの引数を取り、Bool を返す
-- 引数の2つの値の型は Eq 型クラスのインスタンスでなければならない

-- Eq 型クラスは、等値性をテストするためのインターフェイスを提供する
-- ある型の2つの値の等値性を比較することに意味があるなら、その型は Eq 型クラスのインスタンスにできる
-- I/O 型と関数を除く全ての標準型は Eq のインスタンス

-- 2.4.1 Eq 型クラス
-- methods : ==, /=

-- 2.4.2 Ord 型クラス
-- methods : >, <, >=, <=
-- Ordering 型 : GT, LT, EQ

{-
> :t (>)
(>) :: Ord a => a -> a -> Bool

> :t compare
compare :: Ord a => a -> a -> Ordering
-}

-- 2.4.3 Show 型クラス
-- ある型が Show 型クラスのインスタンスになっていれば、文字列として表現できる

{-
> show 3.14
"3.14"
> show True
"True"
-}

-- 2.4.4 Read 型クラス
-- Show と対をなす。文字列を受け取り、Read のインスタンスの型の値を返す

{-
> read "True" || False
True
> read "8.2" + 3.8
12.0

> :t read
read :: Read a => String -> a -- String == [Char]
-}

-- 型注釈
{-
> read "4" :: Int
4
> read "4" :: Float
4.0
> read "(3, 'a')" :: (Int, Char)
(3,'a')
> [read "True", False, True, False]
[True,False,True,False]
-}

-- 2.4.5 Enum 型クラス
-- succ, pred, etc
-- instance : (), Bool, Char, Ordering, Int, Float, Double

{-
> ['a'..'e']
"abcde"
> [LT .. GT]
[LT,EQ,GT]
> [3 .. 5]
[3,4,5]
> succ 'B'
'C'
> pred 'B'
'A'
-}

-- 2.4.6 Bounded 型クラス
-- 有界である

{-
> minBound :: Int
-9223372036854775808
> maxBound :: Char
'\1114111'
> maxBound :: Bool
True
> minBound :: Bool
False
-}

-- 多相定数を持つ
{-
> :t maxBound
maxBound :: Bounded a => a

> maxBound :: (Bool, Int, Char)
(True,9223372036854775807,'\1114111')
-}

-- 2.4.7 Num 型クラス

{-
> :t 20
20 :: Num p => p
> 20 :: Int
20
> 20 :: Integer
20
> 20 :: Float
20.0
> 20 :: Double
20.0
> :t (*)
(*) :: Num a => a -> a -> a
>
-}

-- 2.4.8 Floationg 型クラス

-- 浮動小数点を扱う
-- Float, Double
-- sin, cos, sqrt

-- 2.4.9 Integral 型クラス

-- 整数のみの型クラス
-- Int, Integer

{-
> :t fromIntegral
fromIntegral :: (Integral a, Num b) => a -> b
> :t length
length :: Foldable t => t a -> Int
> fromIntegral (length [1,2,3,4]) + 3.2
7.2
-}

-- 3 関数の構文

-- 3.1 パターンマッチ

first :: (a, b, c) -> a
first (x, _, _) = x
second :: (a, b, c) -> b
second (_, x, _) = x
third :: (a, b, c) -> c
third (_, _, x) = x

-- 3.1.2 リストのパターンマッチとリスト内包表記

{-
> let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
> [a + b | (a, b) <- xs]
[4,7,6,8,11,4]
> [a*100+3 | (a, 3) <- xs]
[103,403,503]
-}

head' :: [a] -> a
head' []      = error "Cant"
head' (x : _) = x

tell :: (Show a) => [a] -> String
tell []       = "The list is empty"
tell (x : []) = "The list has one element: " ++ show x
tell (x : y : []) =
    "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) =
    "This list is long. The first two elements are: "
        ++ show x
        ++ " and "
        ++ show y

-- 3.1.3 as パターン

firstLetter :: String -> String
firstLetter ""           = "Empty string, whoops!"
firstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- 3.2 ガード

bmiTell :: Double -> Double -> String
bmiTell weight height | weight / height ^ 2 <= 18.5 = "underweight"
                      | weight / height ^ 2 <= 25.0 = "normal"
                      | weight / height ^ 2 <= 30.0 = "fat"
                      | otherwise                   = "whale"

max' :: (Ord a) => a -> a -> a
max' a b | a <= b    = b
         | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b | a == b    = EQ
                | a <= b    = LT
                | otherwise = GT

-- 3.3 where?!

bmiTell' :: Double -> Double -> String
bmiTell' weight height | bmi <= skinny = "underweight"
                       | bmi <= normal = "normal"
                       | bmi <= fat    = "fat"
                       | otherwise     = "whale"
  where
    bmi                   = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [ bmi w h | (w, h) <- xs ]
    where bmi weight height = weight / height ^ 2

-- 3.4 let

-- let は式（値を持つ）、where はそうではない

{-
> 4 * (let a = 9 in a + 1) + 2
42
> [let square x = x * x in (square 5, square 3, square 2)]
[(25,9,4)]
> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
(6000000,"Hey there!")
> (let (a, b, c) = (1, 2, 3) in a+b+c) * 100
600
>
-}

-- リスト内包表記のletは、letの後ろから参照できる、前の部分（(w, h) <- xs, ジェネレータ）からは参照できない
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0 ]

-- 3.5 case

head'' :: [a] -> a
head'' []      = error "fail"
head'' (x : _) = x

head''' :: [a] -> a
head''' xs = case xs of
    []      -> error "fail"
    (x : _) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of
    []  -> "empty."
    [x] -> "a singleton list."
    xs  -> "a longer list."

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
  where
    what []  = "empty."
    what [x] = "a singloeton list."
    what xs  = "a longer list."

-- 4 再帰

maximum' :: (Ord a) => [a] -> a
maximum' []       = error "empty."
maximum' [x     ] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x | n <= 0    = []
               | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = [] -- 第二引数は、捨てるため、プレースホルダとして _ を使用。n > 0 のときは次のパターンに移行
take' _ []         = []
take' n (x : xs)   = x : take' (n - 1) xs

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _        []       = []
zip' []       _        = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs) | a == x    = True
                 | otherwise = a `elem'` xs

-- 4.3 クイックソート

-- ピボット（軸）を中心にソート

quicksort :: (Ord a) => [a] -> [a] -- 型シグネチャ
quicksort [] = []
quicksort (x : xs) =
    let smallerOrEqual = [ a | a <- xs, a <= x ]
        larger         = [ a | a <- xs, a > x ]
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

-- 5 高階関数

-- ex. max

{-
> max 4 5
5
> (max 4) 5
5
> :t max
max :: Ord a => a -> a -> a
>
-}

-- max :: Ord a => a -> (a -> a) とも考えられる。（部分適用された関数を返す）

multThree :: Int -> Int -> Int -> Int
-- multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z

{-
> ((multThree 3) 5) 9
135
> multThree 3 5 9
135
-}

multTwoWithNine = multThree 9

{-
> multTwoWithNine 3 5
135
-}

compareWithHundred :: Int -> Ordering
-- compareWithHundred x = compare 100 x
compareWithHundred = compare 100

{-
> compareWithHundred 99
GT
-}

devideByTen :: (Floating a) => a -> a
devideByTen = (/ 10)

{-
> devideByTen 200
20.0
-}

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

-- 5.2 高階実演

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

{-
> applyTwice (+3) 10
16
> applyTwice (++ " HAHA") "HEY"
"HEY HAHA HAHA"
> applyTwice ("HAHA " ++) "HEY"
"HAHA HAHA HEY"
> applyTwice (multThree 2 2) 9
144
> applyTwice (3:) [1]
[3,3,1]
-}

-- 5.2.1 zipWith

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []       _        = []
zipWith' _ _        []       = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

{-
> zipWith' (*) (replicate' 5 2) [1..]
[2,4,6,8,10]
> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6]] [[3,2,2],[3,4,5]]
[[3,4,6],[9,20,30]]
-}

-- 5.2.2 flip

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g where g x y = f y x
flip' f y x = f x y

{-
> flip' zip [1,2,3,4,5] "hello"
[('h',1),('e',2),('l',3),('l',4),('o',5)]
> zipWith' (flip' div) [2,2..] [10,8,6,6,4,2]
[5,4,3,3,2,1]
-}

-- 5.3 tools

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map f xs

{-
> map' (+ 3) [1,2,3]
[4,5,6]
> [x + 3 | x <- [1,2,3]]
[4,5,6]
-}

filter' :: (a -> Bool) -> [a] -> [a]
-- 述部（真理値を返す関数）とリストを受け取り、述部を満たすリストを返す
filter' _ [] = []
filter' p (x : xs) | p x       = x : rest
                   | otherwise = rest
    where rest = filter' p xs

{-
> filter' (< 15) (filter' even [1..20])
[2,4,6,8,10,12,14]
> [x | x <- [1..20], x < 15, even x]
[2,4,6,8,10,12,14]
-}

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) =
    let smallerOrEqual = filter' (<= x) xs
        larger         = filter' (> x) xs
    in  quicksort' smallerOrEqual ++ [x] ++ quicksort' larger

-- 5.3.3 map と filter のさらなる例

largestDivisible :: Integer
largestDivisible = head (filter' (\x -> x `mod` 3829 == 0) [100000, 99999 ..])

{-
> takeWhile (/= ' ') "elephants know how to party"
"elephants"
> sum (takeWhile (< 10000) (filter' odd (map (^ 2) [1..])))
166650
> sum (takeWhile (< 10000) [ m | m <- [ n ^ 2 | n <- [1 ..]], odd m ])
166650
-}


-- コラッツの数列

{-
1. 任意の自然数から開始する
2. 数が1ならば終了
3. 数が偶数なら2で割る
4. 数が奇数なら3倍して1を足す
5. 新しい値でこのアルゴリズムを繰り返す
-}

chain :: Integer -> [Integer]
chain 1 = [1]
chain n | even n    = n : chain (n `div` 2)
        | otherwise = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))

-- 5.3.4 mapに複数の引数を与える

listOfFuns :: [Integer -> Integer]
listOfFuns = map (*) [0 ..]
{-
> (listOfFuns !! 4) 5
20
-}

-- 5.4 lambda expression

{-
> zipWith (\a b -> (a * 30 + 3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]
[153.0,61.5,31.0,15.75,6.6]
-}

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

{-
> zipWith (flip'' (++)) ["love you", "love me"] ["i ", "you "]
["i love you","you love me"]
> map (flip'' subtract 20) [1, 2, 3, 4]
[19,18,17,16]
-}

-- 5.5 畳み込み

{-
> :t foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-}

sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0 -- カリー化された表現
-- foldl f acc [1,2,3] = f (f (f acc 1) 2) 3
-- sum' [3,5,2,1] = 0 + 3 + 5 + 2 + 1

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs
-- foldr f acc [1,2,3] = f 1 (f 2 (f 3 acc))
-- map'' (+ 3) [1,2,3] = (1 + 3):(2 + 3):(3 + 3):[]

map''' :: (a -> b) -> [a] -> [b]
-- ++関数は:よりも遥かに遅いので、リストから新しいリストを構築する際には、右畳み込みを使う
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- 右畳み込みは無限リストに対して動作するが、左畳み込みはだめ

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys
-- elem'' 3 [1, 2, 3, 4, 5] = f 1 (f 2 (f 3 (f 4 (f 5 False))))

-- 5.5.3 foldl1, foldr1

-- lst の先頭（または末尾）を初期アキュームレータとみなし畳み込む

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 max

-- 5.5.4 畳み込みの例

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- product' :: (Num a) => [a] -> a
-- product' = foldl1 (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- 5.5.6 無限リストの畳込み

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- and' [True,False,True] = True && (False && (True && True))

{-
> and' (repeat False)
False
-- False && (False && (False && (False ...
-- 遅延評価なので、False が出てきた時点で計算をやめる
-}

-- 5.5.7 scan

-- scanl, scanr はアキュームレータの中間状態全てをリストとして返す

{-
> scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]
> scanr (+) 0 [3,5,2,1]
[11,8,3,1,0]
-}

-- 自然数の平方根を小さいものから足していったとき、1000を超えるのは何個目？

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

{-
> sqrtSums
131
> sum (map sqrt [1..131])
1005.0942035344083
> sum (map sqrt [1..130])
993.6486803921487
-}

-- 5.6 関数適用演算子

-- def
{-
($) :: (a -> b) -> a -> b
f $ x = f x
-}

-- 普通の関数適用（2つのものの間に空白を置く）は高い優先順位を持つが、$関数は最も低い優先順位を持つ
-- sum (map sqrt [1 .. 130])
-- sum $ map sqrt [1 .. 130]

{-
> map ($ 3) [(4 +), (10 *), (^ 2), sqrt]
[7.0,30.0,9.0,1.7320508075688772]
-}

-- 5.7 関数合成

{-
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
-}

{-
> map (\x -> negate (abs x)) [5, 3, 6, 7, 3, 2, 19, 24]
[-5,-3,-6,-7,-3,-2,-19,-24]
> map (negate . abs) [5, 3, 6, 7, 3, 2, 19, 24]
[-5,-3,-6,-7,-3,-2,-19,-24]
-}

-- 5.7.1 多引数関数の関数合成

{-
sum (replicate 5 (max 6.7 8.9))
(sum . replicate 5) (max 6.7 8.9)
sum . replicate 5 $ max 6.7 8.9
-}

-- 5.7.2 ポイントフリースタイル（ポイント : 一時変数）

sum'' :: (Num a) => [a] -> a
-- sum'' xs = foldl (+) 0 xs
-- カリー化されているので、xsを省略してポイントフリー化
sum'' = foldl (+) 0

fn x = ceiling (negate (tan (cos (max 50 x))))
fn' = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
-- oddSquareSum = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))
oddSquareSum = sum . takeWhile (< 10000) . filter odd $ map (^ 2) [1 ..]
