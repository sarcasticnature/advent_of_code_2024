module Day06 (day06) where

import Control.Monad.ST
import Data.Int (Int8)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

-- Part 1

type Map = VU.Vector Int8

type MutMap s = VU.MVector s Int8

data GuardDir
  = North Int
  | South Int
  | East Int
  | West Int
  | Outside
  deriving (Show)

parseMap :: String -> Map
parseMap cs = runST $ mvec >>= VU.freeze
  where
    cs' = filter (/= '\n') cs
    mvec :: ST s (MutMap s)
    mvec = foldl' f empty $ zip cs' [0 ..]
    f acc (c, i) = do
      vec <- acc
      MVU.write vec i $ if c == '#' then 1 else 0
      return vec
    empty = MVU.new $ length cs' :: ST s (MutMap s)

startDir :: String -> GuardDir
startDir cs =
  let cs' = filter (/= '\n') cs
   in North $ snd $ head $ dropWhile ((/=) '^' . fst) $ zip cs' [0 ..]

walk :: Map -> Int -> GuardDir -> Map
walk m w gd =
  let len = VU.length m
      nextDir gd' = case gd' of
        North i -> if m VU.! i == 0 then North i else East $ i + w
        South i -> if m VU.! i == 0 then South i else West $ i - w
        East i -> if m VU.! i == 0 then East i else South $ i - 1
        West i -> if m VU.! i == 0 then West i else North $ i + 1
        Outside -> error "this should not happen"
      move gd' = case gd' of
        North i -> if i - w < 0 then Outside else nextDir $ North $ i - w
        South i -> if i + w > len then Outside else nextDir $ South $ i + w
        East i -> if (i + 1) `mod` w == 0 || i + 1 > len then Outside else nextDir $ East $ i + 1
        West i -> if (i - 1) `mod` w == w - 1 || i - 1 < 0 then Outside else nextDir $ West $ i - 1
        Outside -> Outside
      go :: (GuardDir, Map) -> (GuardDir, Map) -- ST s (MutMap s)
      go (gd', vec) = if stop then (gd'', vec') else go (gd'', vec')
        where
          gd'' = move gd'
          vec' = case gd' of
            North i -> VU.modify (\v -> MVU.write v i 1) vec
            South i -> VU.modify (\v -> MVU.write v i 1) vec
            East i -> VU.modify (\v -> MVU.write v i 1) vec
            West i -> VU.modify (\v -> MVU.write v i 1) vec
            Outside -> vec
          stop = case gd'' of
            Outside -> True
            _ -> False
      mutM = do
        empty <- (MVU.new len :: ST s (MutMap s)) >>= VU.unsafeFreeze
        return $ snd $ go (gd, empty)
   in runST mutM

-- Part 2

-- Debug

-- prettyMap :: Int -> Map -> String
-- prettyMap width m =
--   let convert n = if n == 1 then 'X' else '.'
--       addNew i = if i == 0 then "\n" else ""
--       f (acc, i) n = (convert n : addNew i ++ acc, (i + 1) `mod` width)
--       rev = fst $ VU.foldl' f ("", 0) m
--    in tail $ reverse rev

day06 :: String -> IO ()
day06 filename = do
  contents <- readFile filename
  let width = length $ head $ lines contents
  let puzzleMap = parseMap contents
  let start = startDir contents
  putStrLn "\nPart 1:"
  -- putStrLn $ prettyMap width puzzleMap
  -- putStrLn "---"
  let outMap = walk puzzleMap width start
  -- putStrLn $ prettyMap width outMap
  print $ VU.foldl' (\a b -> a + fromIntegral b) (0 :: Int) outMap
  putStrLn "\nPart 2:"
