module Day12 (day12) where

import qualified Data.HashMap.Strict as HM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (foldl')
import qualified Data.Vector.Unboxed as VU

-- Part 1

parseMap :: String -> VU.Vector Char
parseMap = VU.fromList . filter (/= '\n')

neighbors :: Int -> Int -> Int -> [Int]
neighbors w h i =
  let north = [i - w | i - w >= 0]
      south = [i + w | i + w < w * h]
      east = [i + 1 | i + 1 < w * h, (i + 1) `mod` w /= 0]
      west = [i - 1 | i - 1 >= 0, (i - 1) `mod` w /= w - 1]
   in north ++ south ++ east ++ west

segment :: Int -> Int -> VU.Vector Char -> HM.HashMap Int IntSet
segment w h cs = fst $ VU.ifoldl' run (HM.empty, IntSet.empty) cs
  where
    flood :: IntSet -> Int -> Char -> IntSet
    flood visited i c =
      let valid i' = (==) c $ cs VU.! i'
          nn = filter (\i' -> IntSet.notMember i' visited && valid i') $ neighbors w h i
          visited' = IntSet.union visited $ IntSet.fromList nn
          f acc i' = flood acc i' c
       in foldl' f visited' nn
    run (hm, visited) i c =
      let new = if IntSet.member i visited then IntSet.empty else flood (IntSet.singleton i) i c
          hm' = if IntSet.member i visited then hm else HM.insert i new hm
       in (hm', IntSet.union new visited)

price :: Int -> Int -> IntSet -> Int
price w h is =
  let area = IntSet.size is
      sides i =
        let nn = neighbors w h i
            f acc n = if IntSet.member n is then acc - 1 else acc
         in foldl' f 4 nn
      perim = IntSet.foldl' (\acc i -> acc + sides i) 0 is
   in area * perim

segmentPrices :: Int -> Int -> HM.HashMap Int IntSet -> [Int]
segmentPrices w h = HM.foldl' f []
  where
    f acc is = price w h is : acc

-- Part 2

day12 :: String -> IO ()
day12 filename = do
  contents <- readFile filename
  let height = length $ lines contents
  let width = length $ head $ lines contents
  putStrLn "\nPart 1:"
  let hm = segment width height $ parseMap contents
  print $ sum $ segmentPrices width height hm
  putStrLn "\nPart 2:"
