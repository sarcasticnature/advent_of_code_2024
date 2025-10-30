module Day05 (day05) where

import Data.List (foldl')

-- Part 1

-- Part 2

day05 :: String -> IO ()
day05 filename = do
    contents <- readFile filename
    let rules = takeWhile (not . null) $ lines contents
    let updates = tail $ dropWhile (not . null) $ lines contents
    putStrLn "Part 1:"
    print $ rules
    print $ updates
    putStrLn "Part 2:"
