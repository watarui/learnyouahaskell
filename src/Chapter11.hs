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
