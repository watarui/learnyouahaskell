module Chapter9 where

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString               as S
import           System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin , newGen  ) = random gen
        (secondCoin, newGen' ) = random newGen
        (thirdCoin , newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

{-
> :l src/Chapter9.hs
[1 of 1] Compiling Chapter9         ( src/Chapter9.hs, interpreted )
Ok, one module loaded.
> threeCoins (mkStdGen 21)
(True,True,True)
> threeCoins (mkStdGen 21)
(True,True,True)
> threeCoins (mkStdGen 22)
(True,False,True)
-}

{-
> take 5 $ randoms (mkStdGen 11) :: [Int]
[5260538044923710387,4361398698747678847,-8221315287270277529,7278185606566790575,1652507602255180489]
> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
> take 5 $ randoms (mkStdGen 11) :: [Float]
[0.26201087,0.1271351,0.31857032,0.1921351,0.31495118]
-}

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value     , newGen  ) = random gen
        (restOfList, finalGen) = finiteRandoms (n - 1) newGen
    in  (value : restOfList, finalGen)

-- randomR' :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)

{-
> randomR (1, 6) (mkStdGen 359353)
(6,1494289578 40692)
> randomR (1, 6) (mkStdGen 35935335)
(3,1250031057 40692)
> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
"xnuhlfwywq"
-}

-- 9.7 bytestring
-- サンク（thunk）: プロミスのままで未評価の部分
-- bytestring: リストににたデータ構造で、要素は1byte or 8bit
-- 遅延bytestringは64Kbyteのchunkという塊に格納する。
-- Word8型: 8ビット符号なし整数 0-255

{-
> B.pack [99,97,110]
"can"
> B.pack [98..120]
"bcdefghijklmnopqrstuvwx"
> by = B.pack [98,111,114,116]
> by
"bort"
> B.unpack by
[98,111,114,116]
> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
"()*+,-./0"
> B.cons 85 $ B.pack [80,81,82,84]
"UPQRT"
-}
