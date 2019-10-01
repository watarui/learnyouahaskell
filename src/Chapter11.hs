module Chapter11 where

-- IOのFunctorインスタンスの実装
{-
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result) -- 結果を提示するI/Oアクションを作る
-}

-- ファンクターとしての関数
{-
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
-}

{-
fmap :: (a -> b) -> f a -> f b
fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
fmap :: (a -> b) -> (r -> a) -> (r -> b) -- r -> a の出力を a -> の入力につなぎ、関数 r -> b を作る。つまり、関数合成
-}

{-
instance Functor ((->) r) where
    fmap = (.)
-}

{-
> :m + Control.Monad.Instances

<interactive>:1:1: warning: [-Wdeprecations]
    Module ‘Control.Monad.Instances’ is deprecated:
      This module now contains no instances and will be removed in the future
> :t fmap (*3) (+100)
fmap (*3) (+100) :: Num b => b -> b
> fmap (*3) (+100) 1
303
> (*3) `fmap` (+100) $ 1
303
> (*3) . (+100) $ 1
303
> fmap (show . (*3)) (+100) 1
"303"
>
-}

-- 関数の持ち上げ lifting

{-
> :t fmap (*2)
fmap (*2) :: (Functor f, Num b) => f b -> f b
> :t fmap (replicate 3)
fmap (replicate 3) :: Functor f => f a -> f [a]
> :set -XNoMonomorphismRestriction
> let shout = fmap (++"!")
> :t shout
shout :: Functor f => f [Char] -> f [Char]
> shout ["ha","ka"]
["ha!","ka!"]
(reverse-i-search)`': fmap (replicate 3)
> fmap (replicate 3) [1,2,3,4]
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
> fmap (replicate 3) (Just 4)
Just [4,4,4]
> fmap (replicate 3) (Right "blah")
Right ["blah","blah","blah"]
> fmap (replicate 3) Nothing
Nothing
> fmap (replicate 3) (Left "foo")
Left "foo"
-}

-- ファンクター則

-- 第一法則
-- idでファンクター値を写した場合、ファンクター値が変化してはいけない
-- fmap id = id

{-
> fmap id (Just 3)
Just 3
> id (Just 3)
Just 3
> fmap id [1..5]
[1,2,3,4,5]
> id [1..5]
[1,2,3,4,5]
> fmap id []
[]
> fmap id Nothing
Nothing
-}

-- Maybeファンクターはファンクター第一法則を満たす
{-
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
-}

-- 第二法則
-- 「fとgの合成関数でファンクター値を写したもの」と「まずg、次にfでファンクター値を写したもの」が等しい
-- fmap (f . g) = fmap f . fmap g
-- fmap (f . g) x = fmap f (fmap g x)

-- 法則を破る

-- C: counter
data CMaybe a = CNothing | CJust Int a deriving (Show)

{-
> CNothing
CNothing
> CJust 0 "haha"
CJust 0 "haha"
> :t CNothing
CNothing :: CMaybe a
> :t CJust 0 "haha"
CJust 0 "haha" :: CMaybe [Char]
> CJust 100 [1,2,3]
CJust 100 [1,2,3]
-}

instance Functor CMaybe where
    fmap f CNothing          = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)

{-
> fmap (++"ha") (CJust 0 "ho")
CJust 1 "hoha"
> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
CJust 2 "hohahe"
> fmap (++"blah") CNothing
CNothing
> fmap id (CJust 0 "haha")
CJust 1 "haha"
> id (CJust 0 "haha")
CJust 0 "haha" -- ファンクター第一法則を満たしていないので、CMaybeはファンクターではない
-}

-- 2引数関数でファンクターを写す場合
-- 関数がファンクター値の中に入っている

{-
> :t fmap (++) (Just "hey")
fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
> :t fmap compare (Just 'a')
fmap compare (Just 'a') :: Maybe (Char -> Ordering)
> :t fmap compare "A LIST OF CHARS"
fmap compare "A LIST OF CHARS" :: [Char -> Ordering]
> :t fmap (\x y z -> x + y / z) [3,4,5,6]
fmap (\x y z -> x + y / z) [3,4,5,6]
  :: Fractional a => [a -> a -> a]
-}

-- 使いみち

{-
> let a = fmap (*) [1,2,3,4]
> :t a
a :: Num a => [a -> a]
> fmap (\f -> f 9) a
[9,18,27,36]
> fmap ($9) a
[9,18,27,36]
-}

-- Applicative Functor f
-- ある型コンストラクタをApplicative型クラスに属させる場合、Functorに属させる型クラス制約k
class (Functor f) => Applicative f where
    -- 任意の型の引数を受け取り、それをアプリカティ値の中に入れて返す。
    pure :: a -> f a
    -- <*> は fmap の強化版
    -- 関数の入っているファンクター値と値の入っているファンクター値を引数にとって
    -- 1つ目のファンクターの中身である関数を2つ目のファンクターの中身に適用する
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    pure = Just
    Nothing  <*> _         = Nothing
    (Just f) <*> something = fmap f something

{-
> Just (+3) <*> Just 9
Just 12
> pure (+3) <*> Just 10
Just 13
> pure (+3) <*> Just 9
Just 12
> Just (++"hahah") <*> Nothing
Nothing
> Nothing <*> Just "woot"
Nothing
-}

-- Applicative style

{-
> pure (+) <*> Just 3 <*> Just 5
Just 8
> pure (+) <*> Just 3 <*> Nothing
Nothing
> pure (+) <*> Nothing <*> Just 5
Nothing
-}


-- pure f <*> x が fmap f x と等しいことを考慮したときに便利な
-- 中置演算子 <$>

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x -- fmap :: (a -> b) -> f a -> f b

{-
> (++) <$> Just "johntra" <*> Just "volta"
Just "johntravolta"
> (++) "johntra" "volta"
"johntravolta"
-}

-- リスト型コンストラクタ []

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [ f x | f <- fs, x <- xs ]

{-
> pure "Hey" :: [String]
["Hey"]
> pure "Hey" :: Maybe String
Just "Hey"
-}

{-
> [(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9]
> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]

> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
[55,80,100,110]
-}

-- IO

instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)


myAction :: IO String
myAction = do
    -- 逐次実行
    a <- getLine
    b <- getLine
    return $ a ++ b

myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

-- 関数 (->) r

instance Applicative ((->) r) where
    pure x = (_ -> x) -- 引数を無視して常にその値を返す関数
    f <*> g = \x -> f x (g x)

{-
> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: Num b => b -> b
-- 引数に (+3) と (*100) を適用して、2つの結果に対して (+) を適用
> (+) <$> (+3) <*> (*100) $ 5
508
> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
-}

-- ZipList

instance Applicative ZipList where
    pure x = ZipList (repeat x) -- pure f <*> xs と fmap f xs を等価にするために必要
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

{-
> import Control.Applicative
> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
[101,102,103]
> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
[101,102,103]
> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
[5,3,3,4]
> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
[('d','c','r'),('o','a','a'),('g','t','t')]
-}

-- アプリカティブ則

-- f <*> x = fmap f x
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u

-- アプリカティブの便利な関数

-- import Control.Applicative

-- 1つの関数を2つのアプリカティブ値に適用する
-- 通常の2引数関数を、2つのアプリカティブ値を引数に取る関数にする
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

{-
> import Control.Applicative
> fmap (\x -> [x]) (Just 4)
Just [4]
> liftA2 (:) (Just 3) (Just [4])
Just [3,4]
-}

-- 「アプリカティブ値のリスト」を取って「リストを返り値として持つ1つのアプリカティブ値」を返す関数
sequenceA :: (Applicative f) => [f a] -> f [a]
-- sequenceA []       = pure []
-- sequenceA (x : xs) = (:) <$> x <*> sequenceA xs
sequenceA = foldr (liftA2 (:)) (pure [])

{-
> import Control.Applicative
> sequenceA [Just 1, Just 2]
Just [1,2]
> sequenceA [(+3),(+2),(+1)] 3
[6,5,4]
> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-}

{-
> and $ map (\f -> f 7) [(>4),(<10),odd]
True
> and $ sequenceA [(>4),(<10),odd] 7
True
-}

{-
-- 「I/Oアクションのリスト」を引数にとって、
-- 「各I/Oアクションを実行し、返り値をリストとして返すI/Oアクション」を返す関数
> sequenceA [getLine, getLine, getLine]
heyh
ho
woo
["heyh","ho","woo"]
-}
