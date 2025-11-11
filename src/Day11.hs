{-# LANGUAGE TupleSections #-}

module Day11 (day11) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- Part 1

split :: Int -> [Int]
split n = [high, low]
  where
    cs = show n
    (highs, lows) = splitAt (length cs `div` 2) cs
    high = read highs
    low = read lows

applyRules :: Int -> [Int]
applyRules n
  | n == 0 = [1]
  | not $ toEnum $ length (show n) `mod` 2 = split n
  | otherwise = [n * 2024]

blink :: IntMap Int -> IntMap Int
blink s =
  let f k v acc = foldr f' acc ks'
        where
          f' k' = IntMap.insertWith (+) k' v
          ks' = applyRules k
   in IntMap.foldrWithKey f IntMap.empty s

sumStones :: IntMap Int -> Int
sumStones = IntMap.foldl' (+) 0

-- Part 2

day11 :: String -> IO ()
day11 filename = do
  contents <- readFile filename
  let stoneList = map read $ words contents
  let stoneMap = IntMap.fromList $ map (,1) stoneList
  putStrLn "\nPart 1:"
  print $ sumStones $ last $ take 26 $ iterate blink stoneMap
  putStrLn "\nPart 2:"
  print $ sumStones $ last $ take 76 $ iterate blink stoneMap
