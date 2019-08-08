module Chapter7 where

import qualified Data.Map                      as Map

-- 7 型や型クラス

{-
data Bool = False | True
-- Shape が型、Circle が値コンストラクタ
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
            deriving (Show) -- Shape型をShow型クラスの一員にする

area :: Shape -> Float
area (Circle _ _ r         ) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-}

{-
> area $ Circle 10 20 10
314.15927
-}

-- 7.2.1 Pointデータ型で形を整える

-- 二次元空間の点を表す中間データ構造を作る
-- 点を定義するとき、データ型と値コンストラクタに同じ名前を使う
-- 値コンストラクタがひとつしかないデータ型ではそうするのが慣例

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) =
    (abs $ x2 - x1) * (abs $ y2 - y1)

-- 図形を動かす関数

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
    Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

-- 座標系の原点に円、長方形を作り、移動させる

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point 0 0) (Point w h)

{-
> area (Rectangle (Point 0 0) (Point 100 100))
10000.0
> area (Circle (Point 0 0) 24)
1809.5574
> nudge (Circle (Point 34 34) 10) 5 10
Circle (Point 39.0 44.0) 10.0
> nudge (baseRect 40 100) 60 23
Rectangle (Point 60.0 23.0) (Point 100.0 123.0)
-}

-- 7.3 レコード構文

{-
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
-}

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

-- 値コンストラクタは引数を取って新しい値を生み出す

{-
> Car {company="Ford", model="Mustang", year=1967}
Car {company = "Ford", model = "Mustang", year = 1967}
-}

-- 7.4 型引数

{-
-- Maybe は a を型引数とする型コンストラクタ
-- 型コンストラクタは、すべての引数を埋めて初めて方になれる
-- ex. Just 'a' は Maybe Char 型と推論される
-- Nothing の型は Maybe a があるので多層的である
data Maybe a = Nothing | Just a
-}

{-
> Just 3 :: Maybe Int
Just 3
-}

-- リスト型は1つの型引数をとって具体型を生成する
-- [Int]型の値、[Char]型の値、[[String]]型の値など

-- 具体型 : 型引数を1つも取らない型か、或いは、型引数を取るけれどもそれが全て埋まっている型
-- ex. Int, Bool, Maybe Char

{-
> Just 3 :: Maybe Int
Just 3
> :t Just "Haha"
Just "Haha" :: Maybe [Char]
> :t Just 84
Just 84 :: Num a => Maybe a
> :t Nothing
Nothing :: Maybe a
> Just 10 :: Maybe Double
Just 10.0
-}

{-
data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)
と定義する意味は殆どない
-}

tellCar :: Car -> String
tellCar (Car { company = c, model = m, year = y }) =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

{-
> let stang = Car {company="Ford", model="Mustang", year=1967}
> tellCar stang
"This Ford Mustang was made in 1967"
-}

-- データ宣言には型クラス制約をつけない

-- 7.4.2 三次元ベクトル

-- ベクトルは多相型にする
-- 普通は数値型に限るとしても、Int, Integer, Doubleなど複数の型をサポートしたいから
-- Int, Integer, FloatはNumのインスタンス
-- データ型宣言時に = の前にあるのが型コンストラクタ、後にあるのが値コンストラクタ
data Vector a = Vector a a a deriving (Show)

-- 和
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

-- 内積
dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i * l + j * m + k * n

-- スカラー倍
vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m)

{-
> Vector 3 5 8 `vplus` Vector 9 2 8
Vector 12 7 16
> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
Vector 12 9 19
> Vector 3 9 7 `vmult` 10
Vector 30 90 70
> Vector 4 9 5 `dotProd` Vector 9.0 2.0 4.0
74.0
> Vector 2 9 3 `vmult` (Vector 4 9 5 `dotProd` Vector 9.0 2.0 4.0)
Vector 148.0 666.0 222.0
-}

-- 7.5 インスタンスの自動導出

-- 特定の型クラスのインスタン宣言を自動導出（derive）する
-- 自動導出できる型クラスは Eq, Ord, Enum, Bounded, Show, Read

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

mikeD = Person { firstName = "Michael", lastName = "Diamond", age = 43 }
adRock = Person { firstName = "Adam", lastName = "Horovitz", age = 41 }
mca = Person { firstName = "Adam", lastName = "Yauch", age = 44 }

{-
-- Eq
> mikeD == adRock
False
> mikeD == mca
False
> mikeD == mikeD
True
> let beastieBoys = [mca, adRock, mikeD]
> mikeD `elem` beastieBoys
True

-- Show
> mikeD
Person {firstName = "Michael", lastName = "Diamond", age = 43}
-}

-- for Read
mysteryDude =
    "Person { firstName =\"Michael\""
        ++ ", lastName =\"Diamond\""
        ++ ", age = 43}"

{-
> read mysteryDude :: Person
Person {firstName = "Michael", lastName = "Diamond", age = 43}
> read mysteryDude == mikeD
True
> read "Just 3" :: Maybe Int
Just 3
-}

-- 7.5.3 Ord

{-
data Bool = False | True deriving (Ord)
> True `compare` False
GT

> Nothing < Just 100
True
> Nothing < Just (-499999)
True
> Just 100 > Just 50
True
-}

-- 7.5.4 Enum, Bounded

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

{-
> Wednesday
Wednesday
> show Wednesday
"Wednesday"
> read "Wednesday" :: Day
Wednesday
> Wednesday == Sunday
False
> Wednesday == Wednesday
True
> Wednesday > Sunday
False
> minBound :: Day
Monday
> maxBound :: Day
Sunday
> succ Monday
Tuesday
> pred Saturday
Friday
> [Wednesday .. Sunday ]
[Wednesday,Thursday,Friday,Saturday,Sunday]
> [minBound  .. maxBound] :: [Day]
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
-}

-- 7.6 型シノニム

-- 型シノニム（型同義名）
-- [Char] と String は同値で交換可能
-- type String = [Char]

-- type PhoneBook = [(String, String)]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
    [ ("betty"  , "555-2938")
    , ("bonnie" , "452-2928")
    , ("patsy"  , "493-2928")
    , ("lucille", "205-2928")
    , ("wendy"  , "939-8282")
    , ("penny"  , "853-2492")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnum pbook = (name, pnum) `elem` pbook

-- 7.6.2 型シノニムの多相化

type AssocList k v = [(k, v)]

-- 型引数を部分適用すると新しい型コンストラクタができる

-- type IntMap v = Map.Map Int v
type IntMap = Map.Map Int

-- 7.6.3 Either

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- 関数がなぜ失敗したのか、どのように失敗したのかを知りたいとき、Either a b型の返り値を使う

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]

{-
> lockerLookup 101 lockers
Right "JAH3I"
> lockerLookup 100 lockers
Left "Locker 100 is already taken!"
> lockerLookup 102 lockers
Left "Locker 102 doesn't exist!"
> lockerLookup 110 lockers
Left "Locker 110 is already taken!"
> lockerLookup 105 lockers
Right "QOTSA"
-}

-- 7.7 再帰的なデータ構造

-- Cons は : を言い換えたもの
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- data List a = Empty | Cons { listhead :: a, listTail :: List a } deriving (Show, Read, Eq, Ord)

{-
> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
Cons 3 (Cons 4 (Cons 5 Empty))
-}

-- 7.7.1 リストの改善

-- 結合性（fixity）宣言
-- ex. * 演算子の結合性は infixl 7 *、+ 演算子の結合性は infixl 6
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

{-
> let a = 3 :-: 4 :-: 5 :-: Empty
> 100 :-: a
100 :-: (3 :-: (4 :-: (5 :-: Empty)))
-}

{-
-- 標準のリストにおける ++ の定義
infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]       ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)
-}

-- ^++ を作る
infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty      ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)
-- パターンマッチは値コンストラクタに対して行われる

{-
> let a = 3 :-: 4 :-: 5 :-: Empty
> let b = 6 :-: 7 :-: Empty
> a ^++ b
3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))
-}

-- 7.7.2 二分探索木

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) | -- 空の木
                                   x == a = Node x left right
                                 | -- 左部分木
                                   x < a  = Node a (treeInsert x left) right
                                 | -- 右部分木
                                   x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right) | x == a = True
                               | x < a  = treeElem x left
                               | x > a  = treeElem x right

{-
> let nums = [8,6,4,1,7,3,5]
> let numsTree = foldr treeInsert EmptyTree nums
> numsTree
Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))
-}

{-
> 8 `treeElem` numsTree
True
> 100 `treeElem` numsTree
False
-}

-- 7.8 型クラス 中級講座

-- 7.8.1 Eq 型クラスの内部

{-
-- 標準ライブラリにおけるEqの定義
class Eq a where -- 新しい型クラスの定義を記述
    -- 相互再帰という形でのデフォルト実装
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
-}

-- 7.8.2 交通信号データ型

data TrafficLight = Red | Yellow | Green

-- TrafficLight 型を Eq 型クラスのインスタンスにする
instance Eq TrafficLight where
    -- 型クラスの最小完全主義（minimal complete definition）
    Red    == Red    = True
    Green  == Green  = True
    Yellow == Yellow = True
    _      == _      = False

instance Show TrafficLight where
    show Red    = "Red light"
    show Yellow = "Yellow light"
    show Green  = "Green light"

-- 7.8.3 サブクラス化

{-
class (Eq a) => Num a where
    ...
-}

-- 7.8.4 多相型を型クラスのインスタンスに

-- Maybe は型引数を1つ取って具体型を生み出す型コンストラクタ

-- 任意の m に対して Maybe m となる型を Eq のインスタンスにする
{-
instance (Eq m) => Eq (Maybe m) where
    Just x  == Just y  = x == y
    Nothing == Nothing = True
    _       == _       = False
-}

{-
> :i Maybe
data Maybe a = Nothing | Just a         -- Defined in ‘GHC.Maybe’
instance Applicative Maybe -- Defined in ‘GHC.Base’
instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Maybe’
instance Functor Maybe -- Defined in ‘GHC.Base’
instance Monad Maybe -- Defined in ‘GHC.Base’
instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
instance Ord a => Ord (Maybe a) -- Defined in ‘GHC.Maybe’
instance Semigroup a => Semigroup (Maybe a)
  -- Defined in ‘GHC.Base’
instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
instance Read a => Read (Maybe a) -- Defined in ‘GHC.Read’
instance Foldable Maybe -- Defined in ‘Data.Foldable’
instance Traversable Maybe -- Defined in ‘Data.Traversable’
-}

-- 7.9 Yes, No

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo Bool where
    yesno = id -- 引数を1つ取って同じものを返す標準ライブラリ関数

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing  = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _         = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _   = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal then yesResult else noResult

-- 7.10 Functor 型クラス

-- 写像 ex. map

{-
class Functor f where -- f は具体型ではなく型コンストラクタ
    fmap :: (a -> b) -> f a -> f b
-}

{-
instance Functor [] where -- [] は型コンストラクタ
    fmap = map
-}

-- 7.10.1 Maybe

{-
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing  = Nothing
-}

{-
> fmap (*2) (Just 200)
Just 400
> fmap (*2) Nothing
Nothing
-}

-- 7.10.2 Tree

instance Functor Tree where
    fmap f EmptyTree           = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

{-
> foldr treeInsert EmptyTree [5,7,3]
Node 3 EmptyTree (Node 7 (Node 5 EmptyTree EmptyTree) EmptyTree)
-}

-- 7.10.3 Either

{-
-- in Control.Monad.Instances
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left  x) = Left x
-}

-- 7.11 種類（kind）

{-
> :k Int
Int :: *
-- * は具体型を表す
> :k Maybe
Maybe :: * -> *
-- 1つの具体型を引数にとって、具体型を返す型コンストラクタ
> :k Either
Either :: * -> * -> *
> :k Either String
Either String :: * -> *
> :k Either String Int
Either String Int :: *
-}
