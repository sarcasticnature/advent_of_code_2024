import System.Environment (getArgs)
import System.IO
import Data.List (foldl', nub)
import qualified Data.HashMap.Strict as HM

type Index = (Int, Int)
type TowerMap = HM.HashMap Char [Index]

-- Part 1

index :: [String] -> (String, [Index])
index cs =
    let height = length cs
        width = length $ head cs
        run n = zip [0..width - 1] $ repeat n
    in  (concat cs, concatMap run [0..height - 1])

collectTowers :: (String, [Index]) -> TowerMap
collectTowers (cs, idxs) =
    let zs = zip cs idxs
        f acc (c, idx) = HM.insertWith (++) c [idx] acc
    in  HM.delete '.' $ foldl' f HM.empty zs

doubles :: Index -> Index -> [Index]
doubles (x1, y1) (x2, y2) =
    if run > 0
    then [(x2 + run, y2 + rise), (x1 - run, y1 - rise)]
    else [(x1 - run, y1 - rise), (x2 + run, y2 + rise)]
    where rise = y2 - y1
          run = x2 - x1

computeAntiNodes :: [Index] -> [Index] -> [Index]
computeAntiNodes acc [] = acc
computeAntiNodes acc (t:ts) =
    let as = concatMap (doubles t) ts
    in  computeAntiNodes (as ++ acc) ts

findAntiNodes :: TowerMap -> [Index]
findAntiNodes = HM.foldl' f []
    where f acc idxs = computeAntiNodes [] idxs ++ acc

withinBounds :: Int -> Int -> (Int, Int) -> Bool
withinBounds w h (x,y) =
    let below = x < w && y < h
        above = x >= 0 && y >= 0
    in  below && above

-- Part 2

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    let l = lines contents
    let height = length l
    let width = length $ head l
    print $ length $ nub $ filter (withinBounds width height) $ findAntiNodes $ collectTowers $ index $ lines contents
    putStrLn "Part 2:"
