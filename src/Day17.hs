module Day17 (day17) where

import Control.Monad.State
import Data.Bits (xor)
import Data.List (isPrefixOf)

-- Part 1

-- TODO: hardcoding instruction count
-- instructionCount = 3 :: Int
instructionCount = 8 :: Int

data Registers = Registers
  { regA :: Int,
    regB :: Int,
    regC :: Int,
    ip :: Int,
    tx :: String
  }
  deriving (Show)

type Program = [(Int, Int)]

type ComputerState = State (Registers, Program) ()

splitOn :: Char -> String -> [String]
splitOn x cs = case dropWhile (x ==) cs of
  "" -> []
  cs' -> c : splitOn x cs''
    where
      (c, cs'') = break (x ==) cs'

parse :: String -> ComputerState
parse cs =
  let ls = lines cs
      ra = read $ drop 12 $ head ls
      rb = read $ drop 12 $ ls !! 1
      rc = read $ drop 12 $ ls !! 2
      ns = map read $ splitOn ',' $ drop 9 $ last ls
      chunk [] = []
      chunk (o : n : xs) = (o, n) : chunk xs
   in state $ const ((), (Registers ra rb rc 0 [], chunk ns))

execute :: (Registers, Program) -> (Registers, Program)
execute (Registers a b c ip tx, is)
  | op == 0 = (Registers dv b c (ip + 1) tx, is)
  | op == 1 = (Registers a bxl c (ip + 1) tx, is)
  | op == 2 = (Registers a bst c (ip + 1) tx, is)
  | op == 3 = (Registers a b c jnz tx, is)
  | op == 4 = (Registers a bxc c (ip + 1) tx, is)
  | op == 5 = (Registers a b c (ip + 1) out, is)
  | op == 6 = (Registers a dv c (ip + 1) tx, is)
  | op == 7 = (Registers a b dv (ip + 1) tx, is)
  where
    (op, dat) = is !! ip
    combo n
      | n <= 3 = n
      | n == 4 = a
      | n == 5 = b
      | n == 6 = c
    dv = div a $ 2 ^ combo dat
    bxl = b `xor` dat
    bst = combo dat `mod` 8
    jnz = if a /= 0 then dat else ip + 1
    bxc = b `xor` c
    outDat = show $ combo dat `mod` 8
    out = if null tx then outDat else tx ++ (',' : outDat)

run :: ComputerState
run = do
  cs <- get
  put $ execute cs

step :: (Registers, Program) -> (Registers, Program)
step = execState run

-- Part 2

getInstructions :: String -> String
getInstructions = drop 9 . last . lines

parseInstructions :: String -> Program
parseInstructions cs =
  let ns = map read $ splitOn ',' cs
      chunk [] = []
      chunk (o : n : xs) = (o, n) : chunk xs
   in chunk ns

increment :: (Registers, Program) -> (Registers, Program)
increment (Registers a b c ip tx, p) = (Registers (a + 1) b c ip tx, p)

valid :: String -> Registers -> Bool
valid cs (Registers a b c ip tx) = cs == tx -- && ip >= instructionCount

day17 :: String -> IO ()
day17 filename = do
  contents <- readFile filename

  putStrLn "Part 1:"
  let initialState = execState (parse contents) (Registers 0 0 0 0 [], [])
  let pred = (>) instructionCount . ip . fst
  print $ tx $ fst $ last $ takeWhile pred $ iterate step initialState

  putStrLn "Part 2:"
  let instructions = getInstructions contents
  let initialState' = (Registers 0 0 0 0 [], parseInstructions instructions)
  let pred' = flip isPrefixOf instructions . tx . fst
  -- let step' = fst . last . takeWhile pred . iterate step
  let step' = fst . last . takeWhile (\x -> pred' x && pred x) . iterate step
  -- print $ head $ drop 117440 $ iterate increment initialState'
  -- print $ head $ map (valid instructions . step') $ drop 117440 $ iterate increment initialState'
  -- print $ fst $ head $ dropWhile (not . valid instructions . snd) $ zip [0..] $ map step' $ iterate increment initialState'
  mapM_ (print . fst) $ zip [0 ..] $ map step' $ iterate increment initialState'

-- mapM_ print $ zip [0..] $ map (not . valid instructions . step') $ iterate increment initialState'
