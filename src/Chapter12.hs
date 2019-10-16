module Chapter12 where

import Data.Monoid

-- Monoid

{-
data : 独自の代数データ型を作る
type : 既存の型に型シノニムを与える
newtype : 既存の型から新たな型をつくる
          値コンストラクタは1種類しか作れず、
          その値コンストラクタが持てるフィールドも1つだけという制限がある。
-}

-- 等号が使え、文字列に変換できる
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
-- CharList :: [Char] -> CharList
-- getCharList :: CharList -> [Char]

{-
> CharList "this will be shown!"
CharList {getCharList = "this will be shown!"}
> CharList "benny" == CharList "benny"
True
> CharList "benny" == CharList "others"
False
-}

{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- 型変数が1つの型コンストラクタだけが Functor のインスタンスになれる
instance Functor Maybe where
-}

newtype Pair b a = Pair { getPair :: (a, b) }
instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y) -- パターンマッチを使って Pair 型から中身のタプルを取り出す

{-
> getPair $ fmap (*100) (Pair (2, 3))
(200,3)
> getPair $ fmap reverse (Pair ("london calling", 3))
("gnillac nodnol",3)
-}

newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

{-
> helloMe undefined
"hello"
-}

-- type, newtype, data

-- type
-- 既存の型に別名をつけた型シノニムを作る
-- 型注釈にどちらを使うかは自由
type IntList = [Int]

{-
> ([1,2,3]::IntList)++([1,2,3]::[Int])
[1,2,3,1,2,3]
-}

-- newtype
-- 既存の型を包んで新しい型を作る
-- 型クラスのインスタンスを作りやすくする
-- 値コンストラクタが一つだけ、フィールドも一つだけという制限のついたdata宣言ともみなせる
-- newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

-- data
-- 自作の新しいデータ型を作る

-- Monoid型クラス
-- 演算 * と ++ は結合的（associativity）
-- モノイドは結合的な二項演算子と、その演算に関する単位元からなる構造。
-- ex. 1 は * の単位元、 [] は ++ の単位元

{-
-- Defined by Data.Monoid
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat =  foldr mappend mempty
-}

-- Monoid のインスタンスになれるのは具体型だけ
-- mempty は多相定数で、そのモノイドの単位元を表す
-- mappend モノイド固有の二項演算
-- mconcat モノイドのリストを取って mappend を間に挟んだ式を作り、単一の値を計算する

-- モノイド則

-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- モノイドとしてのリスト
{-
instance Monoid [a] where
    mempty = []
    mappend = (++)
-}

-- Product in Data.Monoid
{-
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)
-}

-- Sum
{-
newtype Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)
-}

-- Any
{-
newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)
-}

-- All
{-
newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
    mempty = All False
    All x `mappend` All y = All (x && y)
-}

-- Ordering
{-
instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
-}

lengthCompare :: String -> String -> Ordering
{-
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in if a == EQ then b else a
-}
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")
