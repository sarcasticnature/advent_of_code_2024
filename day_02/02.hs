import System.Environment (getArgs)
import System.IO
import Data.List (foldl')

-- Part 1

levelSafe :: Bool -> Int -> Int -> Bool
levelSafe up a b = diff > 0 && diff <= 3 && if up then b > a else b < a
    where diff = abs $ a - b

-- TODO: cut to the chase and just return an Int?
reportSafe :: String -> Bool
reportSafe cs =
    let ls = map read $ words cs :: [Int]
        up = head ls < (ls !! 1)
    in  and $ zipWith (levelSafe up) ls $ tail ls

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ foldl' (flip ((+) . fromEnum)) 0 $ map reportSafe $ lines contents
    putStrLn "Part 2:"
