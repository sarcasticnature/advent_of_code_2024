module Day12 (day12) where

import qualified Data.HashMap.Strict as HM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (foldl', sortBy)
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
      freeSides i =
        let nn = neighbors w h i
            f acc n = if IntSet.member n is then acc - 1 else acc
         in foldl' f 4 nn
      perim = IntSet.foldl' (\acc i -> acc + freeSides i) 0 is
   in area * perim

segmentPrices :: Int -> Int -> HM.HashMap Int IntSet -> [Int]
segmentPrices w h = HM.foldl' f []
  where
    f acc is = price w h is : acc

-- Part 2

verticalSides :: Int -> Int -> VU.Vector Char -> Int -> (Bool, Bool)
verticalSides w h cs i =
  let char = cs VU.! i
      north = i - w < 0 || char /= (cs VU.! (i - w))
      south = i + w >= w * h || char /= (cs VU.! (i + w))
   in (north, south)

horizontalSides :: Int -> Int -> VU.Vector Char -> Int -> (Bool, Bool)
horizontalSides w h cs i =
  let char = cs VU.! i
      west = i - 1 < 0 || (i - 1) `mod` w == w - 1 || char /= (cs VU.! (i - 1))
      east = i + 1 >= w * h || (i + 1) `mod` w == 0 || char /= (cs VU.! (i + 1))
   in (west, east)

sides :: Int -> Int -> VU.Vector Char -> IntSet -> Int
sides w h cs is =
  let iList = IntSet.elems is
      go_h (tb, acc) [] = (tb, acc)
      go_h ((t, b), acc) (i : il) =
        let (t', b') = verticalSides w h cs i
            topSide = fromEnum $ t' && not t
            bottomSide = fromEnum $ b' && not b
            hasNext = (i + 1) `mod` w /= 0 && IntSet.member (i + 1) is
            (t'', b'') = if hasNext then (t', b') else (False, False)
         in go_h ((t'', b''), acc + topSide + bottomSide) il
      scan_h = snd $ go_h ((False, False), 0) iList
      vertSort i_a i_b
        | mod_a < mod_b = LT
        | mod_a > mod_b = GT
        | i_a < i_b = LT
        | i_a > i_b = GT
        | otherwise = EQ
        where
          mod_a = i_a `mod` w
          mod_b = i_b `mod` w
      go_v (lr, acc) [] = (lr, acc)
      go_v ((l, r), acc) (i : il) =
        let (l', r') = horizontalSides w h cs i
            leftSide = fromEnum $ l' && not l
            rightSide = fromEnum $ r' && not r
            hasNext = (i + w) < w * h && IntSet.member (i + w) is
            (l'', r'') = if hasNext then (l', r') else (False, False)
         in go_v ((l'', r''), acc + leftSide + rightSide) il
      scan_v = snd $ go_v ((False, False), 0) $ sortBy vertSort iList
   in IntSet.size is * (scan_h + scan_v)

day12 :: String -> IO ()
day12 filename = do
  contents <- readFile filename
  let height = length $ lines contents
  let width = length $ head $ lines contents
  putStrLn "\nPart 1:"
  let gardenMap = parseMap contents
  let hm = segment width height gardenMap
  print $ sum $ segmentPrices width height hm
  putStrLn "\nPart 2:"
  print $ sum $ map (sides width height gardenMap . snd) $ HM.toList hm
