module Day15 (day15) where

import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

-- Part 1

data Move = North | South | East | West deriving (Show)

parseMap :: String -> (VU.Vector Char, Int)
parseMap cs =
  let cs' = concat $ takeWhile (/= "") $ lines cs
      vec = VU.fromList cs'
      robotIdx = fromJust $ VU.findIndex (== '@') vec
   in (VU.modify (\v -> MVU.write v robotIdx '.') vec, robotIdx)

parseMoves :: String -> [Move]
parseMoves cs =
  let cs' = concat $ tail $ dropWhile (/= "") $ lines cs
      parse c
        | c == '^' = North
        | c == 'v' = South
        | c == '>' = East
        | c == '<' = West
        | otherwise = error $ "Unexpected char '" ++ [c] ++ "' when parsing direction"
   in map parse cs'

move :: Int -> Int -> (VU.Vector Char, Int) -> Move -> (VU.Vector Char, Int)
move w h (vec, i) m =
  let len = w * h
      path = case m of
        North -> takeWhile (>= 0) [i - w, i - 2 * w ..]
        South -> takeWhile (< len) [i + w, i + 2 * w ..]
        East -> takeWhile (\x -> x `mod` w /= 0 && x < len) [i + 1 ..]
        West -> takeWhile (\x -> x `mod` w /= w - 1 && x >= 0) [i - 1, i - 2 ..]
      go :: Int -> [Int] -> Maybe Int
      go _ [] = error "reached the end of a path without hitting a wall"
      go acc (i' : is)
        | c == '#' = Nothing
        | c == 'O' = go (acc + 1) is
        | c == '.' = Just acc
        | otherwise = error $ "bad character '" ++ [c] ++ "' in map"
        where
          c = vec VU.! i'
   in case go 0 path of
        Nothing -> (vec, i)
        Just n -> (vec'', i')
          where
            f v idx = VU.modify (\mv -> MVU.write mv idx 'O') v
            vec' = VU.modify (\mv -> MVU.write mv (head path) '.') vec
            vec'' = foldl' f vec' $ take n $ tail path
            i' = if not $ null path then head path else i

score :: Int -> VU.Vector Char -> Int
score w = VU.ifoldl' f 0
  where
    f acc i c =
      let (row, col) = i `divMod` w
          gps = 100 * row + col
       in if c == 'O' then acc + gps else acc

-- Part 2

-- Debug

-- prettyPrint :: Int -> (VU.Vector Char, Int) -> IO ()
-- prettyPrint w (vec, i) = putStrLn $ tail $ reverse $ VU.ifoldl' f [] vec'
--   where
--     f acc i' c = if i' `mod` w == 0 then c : '\n' : acc else c : acc
--     vec' = VU.modify (\v -> MVU.write v i '@') vec

day15 :: String -> IO ()
day15 filename = do
  contents <- readFile filename
  let height = length $ takeWhile (/= "") $ lines contents
  let width = length $ head $ lines contents
  putStrLn "\nPart 1:"
  print $ score width $ fst $ foldl' (move width height) (parseMap contents) $ parseMoves contents
  -- prettyPrint width $ foldl' (move width height) (parseMap contents) $ parseMoves contents
  -- mapM_ (prettyPrint width) $ scanl (move width height) (roboMap, start) $ parseMoves contents
  putStrLn "\nPart 2:"
