import           Data.List

-- 10.2 ヒースロー空港からロンドンへの最短経路

{-
A - 50 - A1 -  5 - A2 - 40 - A3 - 10 - A4
          |         |         |         |
         30        20        25         0
          |         |         |         |
B - 10 - B1 - 90 - B2 -  2 - B3 -  8 - B4
-}

main = do
    contents <- getContents
    let threes     = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a, b, c] -> Section a b c) threes
        path       = optimalPath roadSystem
        pathString = concatMap (show . fst) path
        pathTime   = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime


data Section = Section { getA :: Int, getB :: Int, getC :: Int }
    deriving (Show)
type RoadSystem = [Section]


heathrowToLondon :: RoadSystem
heathrowToLondon =
    [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]


data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]


roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA          = sum $ map snd pathA -- 最適経路の所要時間
        timeB          = sum $ map snd pathB
        forwardTimeToA = timeA + a
        crossTimeToA   = timeB + b + c
        forwardTimeToB = timeB + b
        crossTimeToB   = timeA + a + c
        newPathToA     = if forwardTimeToA <= crossTimeToA
            then (A, a) : pathA
            else (C, c) : (B, b) : pathB
        newPathToB = if forwardTimeToB <= crossTimeToB
            then (B, b) : pathB
            else (C, c) : (A, a) : pathA
    in  (newPathToA, newPathToB)


optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath


-- ex. groupsOf 3 [1..10] => [[1,2,3],[4,5,6],[7,8,9],[10]]
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
