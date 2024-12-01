import System.Environment (getArgs)
import System.IO
import Data.List (sort, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseLine :: String -> ([Int], [Int]) -> ([Int], [Int])
parseLine cs (ls, rs) = (l:ls, r:rs)
    where ws = words cs
          l = read $ head ws
          r = read $ last ws

parseLists :: [String] -> ([Int], [Int])
parseLists = foldr parseLine ([], [])

-- TODO: use Arrows???
sortLists :: ([Int], [Int]) -> ([Int], [Int])
sortLists (ls, rs) = (sort ls, sort rs)

distances :: ([Int], [Int]) -> Int
distances (ls, rs) = sum $ zipWith (\x y -> abs $ x - y) ls rs

-- TODO: yes this is inefficient
mapper (rs', m) l = let cnt = length $ takeWhile (l ==) $ dropWhile (l >) rs'
                        m' = Map.insertWith (+) l cnt m
                        rs'' = dropWhile (l >) rs'
                    in  (rs'', m')

buildMap :: ([Int], [Int]) -> Map Int Int
buildMap (ls, rs) = snd $ foldl' mapper (rs, Map.empty) ls

crunchMap :: Map Int Int -> Int
crunchMap = Map.foldlWithKey' (\acc k v -> acc + k * v) 0

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    let sorted = sortLists $ parseLists $ lines contents
    putStrLn "Part 1:"
    print $ distances $ sorted
    putStrLn "Part 2:"
    -- TODO: combine parsing
    print $ crunchMap $ buildMap $ sorted
