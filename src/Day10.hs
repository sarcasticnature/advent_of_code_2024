module Day10 (day10) where

import Data.Char (digitToInt)
import Data.Int (Int8)
import Data.List (nub)
import qualified Data.Vector.Unboxed as VU

-- Part 1

type VecMap = VU.Vector Int8

parseVec :: String -> VecMap
parseVec = VU.fromList . map (fromIntegral . digitToInt) . filter (/= '\n')

neighbors :: Int -> Int -> Int -> [Int]
neighbors w h i =
  let north = [i - w | i - w >= 0]
      south = [i + w | i + w < w * h]
      east = [i + 1 | i + 1 < w * h, (i + 1) `mod` w /= 0]
      west = [i - 1 | i - 1 >= 0, (i - 1) `mod` w /= w - 1]
   in north ++ south ++ east ++ west

findTrails :: Int -> Int -> VecMap -> [Int]
findTrails w h vm = VU.ifoldl' f [] vm
  where
    at = (VU.!) vm
    run 9 i = [i]
    run n i =
      let nn = filter ((==) (n + 1) . at) $ neighbors w h i
       in nub $ concatMap (run $ n + 1) nn
    f acc i n = if n == 0 then length (run 0 i) : acc else acc

-- Part 2

findRating :: Int -> Int -> VecMap -> [Int]
findRating w h vm = VU.ifoldl' f [] vm
  where
    at = (VU.!) vm
    run 9 i = [i]
    run n i =
      let nn = filter ((==) (n + 1) . at) $ neighbors w h i
       in concatMap (run $ n + 1) nn
    f acc i n = if n == 0 then length (run 0 i) : acc else acc

day10 :: String -> IO ()
day10 filename = do
  contents <- readFile filename
  let height = length $ lines contents
  let width = length $ head $ lines contents
  let vecMap = parseVec contents
  putStrLn "\nPart 1:"
  print $ sum $ findTrails width height vecMap
  putStrLn "\nPart 2:"
  print $ sum $ findRating width height vecMap
