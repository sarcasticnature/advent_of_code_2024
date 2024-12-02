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

-- Part 2

parseList :: String -> [Int]
parseList = map read . words

listSafe :: [Int] -> Bool
listSafe ls =
    let up = head ls < (ls !! 1)
        safes = zipWith (levelSafe up) ls $ tail ls
        badCount = foldl' (\acc x -> if x then acc + 1 else acc) 0 safes
        good = map snd $ takeWhile fst $ zip safes ls
        badIdx = length good
        firstDamp = listSafe' $ good ++ drop (badIdx + 1) ls
        secondDamp = listSafe' $ good ++ [ls !! badIdx] ++ drop (badIdx + 2) ls
        dropFirst = listSafe' $ drop 1 ls
    in  and safes || firstDamp || secondDamp || dropFirst

listSafe' :: [Int] -> Bool
listSafe' ls =
    let up = head ls < (ls !! 1)
    in  and $ zipWith (levelSafe up) ls $ tail ls

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ foldl' (flip ((+) . fromEnum)) 0 $ map reportSafe $ lines contents
    putStrLn "Part 2:"
    print $ foldl' (flip ((+) . fromEnum)) 0 $ map (listSafe . parseList) $ lines contents
