module Day06 (day06) where

import Control.Monad.ST
import Data.Int (Int8)
import Data.List (foldl', nub)
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
  deriving (Show, Eq)

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

nextDir :: Map -> Int -> GuardDir -> GuardDir
nextDir m w gd' = case gd' of
  North i -> if m VU.! i == 0 then North i else East $ i + w
  South i -> if m VU.! i == 0 then South i else West $ i - w
  East i -> if m VU.! i == 0 then East i else South $ i - 1
  West i -> if m VU.! i == 0 then West i else North $ i + 1
  Outside -> error "this should not happen"

step :: Map -> Int -> GuardDir -> GuardDir
step m w gd' = case gd' of
  North i -> if (i - w) < 0 then Outside else next $ North $ i - w
  South i -> if (i + w) >= len then Outside else next $ South $ i + w
  East i -> if (i + 1) `mod` w == 0 || i + 1 >= len then Outside else next $ East $ i + 1
  West i -> if (i - 1) `mod` w == w - 1 || i - 1 < 0 then Outside else next $ West $ i - 1
  Outside -> Outside
  where
    next = nextDir m w
    len = VU.length m

outside :: GuardDir -> Bool
outside gd = case gd of
  Outside -> True
  _ -> False

walk :: Map -> Int -> GuardDir -> Map
walk m w gd =
  let len = VU.length m
      go :: (GuardDir, Map) -> (GuardDir, Map)
      go (gd', vec) = if stop then (gd'', vec') else go (gd'', vec')
        where
          gd'' = step m w gd'
          vec' = case gd' of
            North i -> VU.modify (\v -> MVU.write v i 1) vec
            South i -> VU.modify (\v -> MVU.write v i 1) vec
            East i -> VU.modify (\v -> MVU.write v i 1) vec
            West i -> VU.modify (\v -> MVU.write v i 1) vec
            Outside -> vec
          stop = outside gd''
      mutM = do
        empty <- (MVU.new len :: ST s (MutMap s)) >>= VU.unsafeFreeze
        return $ snd $ go (gd, empty)
   in runST mutM

-- Part 2

walkList :: Map -> Int -> GuardDir -> [GuardDir]
walkList m w gd =
  let go :: (GuardDir, [GuardDir]) -> (GuardDir, [GuardDir])
      go (gd', gs) = if stop then (gd'', gs') else go (gd'', gs')
        where
          gd'' = step m w gd'
          gs' = gd' : gs
          stop = outside gd''
   in snd $ go (gd, [])

findLoops :: Map -> Int -> GuardDir -> [GuardDir] -> [Int]
findLoops m w start gs =
  let isLoop m' gd acc
        | gd' `elem` acc = True
        | outside gd' = False
        | otherwise = isLoop m' gd' $ gd : acc
        where
          gd' = step m' w gd
      f acc gd = if unblocked && isLoop m' start [] then idx : acc else acc
        where
          gd' = step m w gd
          (m', unblocked) = case (gd, gd') of
            (North _, North i) -> (VU.modify (\v -> MVU.write v i 1) m, True)
            (South _, South i) -> (VU.modify (\v -> MVU.write v i 1) m, True)
            (East _, East i) -> (VU.modify (\v -> MVU.write v i 1) m, True)
            (West _, West i) -> (VU.modify (\v -> MVU.write v i 1) m, True)
            _ -> (m, False)
          idx = case gd' of
            North i -> i
            South i -> i
            East i -> i
            West i -> i
            Outside -> -1
   in foldl' f [] gs

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
  print $ length $ nub $ findLoops puzzleMap width start $ tail $ walkList puzzleMap width start
