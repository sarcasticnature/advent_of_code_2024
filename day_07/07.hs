import System.Environment (getArgs)
import System.IO
import Data.List (nub)

-- Part 1

combos :: [Int] -> [Int] -> [Int]
combos acc [] = acc
combos [] (n:ns) = combos [n] ns
combos acc (n:ns) = combos (map (n +) acc ++ map (n *) acc) ns

equationCount :: String -> [Int]
equationCount cs =
    let cs' = words cs
        ns = map read $ tail cs'
        goal = read $ init $ head cs'
        options = combos [] ns
    in  nub $ filter (goal ==) options

-- Part 2

combos' :: [Int] -> [Int] -> [Int]
combos' acc [] = acc
combos' [] (n:ns) = combos' [n] ns
combos' acc (n:ns) = combos' (map (n +) acc ++ map (n *) acc ++ map (glue n) acc) ns
    where glue a b = read $ show b ++ show a

equationCount' :: String -> [Int]
equationCount' cs =
    let cs' = words cs
        ns = map read $ tail cs'
        goal = read $ init $ head cs'
        options = combos' [] ns :: [Int]
    in  nub $ filter (goal ==) options

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ sum $ concatMap equationCount $ lines contents
    putStrLn "Part 2:"
    print $ sum $ concatMap equationCount' $ lines contents
