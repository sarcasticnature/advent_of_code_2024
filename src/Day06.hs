module Day06 (day06) where

import Control.Monad.ST
import Data.Int (Int8)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

-- Part 1

type Map = VU.Vector Int8

type MutMap s = VU.MVector s Int8

index :: Int -> Int -> Int -> Int
index size x y = y * size + x

parseMap :: String -> Map
parseMap cs = runST $ mvec >>= VU.freeze
  where
    mvec :: ST s (MutMap s)
    mvec = foldl' f empty $ zip cs [0 :: Int ..]
    f acc (c, i) = do
      vec <- acc
      MVU.write vec i $ if c == '#' then 1 else 0
      return vec
    empty = MVU.new $ length cs :: ST s (MutMap s)

-- Part 2

day06 :: String -> IO ()
day06 filename = do
  contents <- readFile filename
  putStrLn "Part 1:"
  print $ parseMap contents
  putStrLn "Part 2:"
