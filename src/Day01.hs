module Day01 (day01) where

import Data.List (sort, foldl')
import Data.Bifunctor (bimap)

parseLine :: String -> (Int, Int)
parseLine cs = (l, r)
    where ws = words cs
          l = read $ head ws
          r = read $ last ws

folder :: ([Int], Int) -> Int -> ([Int], Int)
folder (rs, acc) l = let cnt = length $ takeWhile (l ==) $ dropWhile (l >) rs
                         rs' = dropWhile (l >) rs
                     in  (rs', acc + l * cnt)

similarity :: ([Int], [Int]) -> Int
similarity (ls, rs) = snd $ foldl' folder (rs, 0) ls

day01 :: String -> IO ()
day01 filename = do
    contents <- readFile filename
    let sorted = bimap sort sort $ unzip $ map parseLine $ lines contents
    putStrLn "Part 1:"
    print $ sum $ uncurry (zipWith (\x y -> abs $ x - y)) sorted
    putStrLn "Part 2:"
    print $ similarity sorted
