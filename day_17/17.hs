import System.Environment (getArgs)
import System.IO
import Control.Monad.State
import Data.Bits (xor)

-- Part 1

-- TODO: hardcoding instruction count
--instructionCount = 3 :: Int
instructionCount = 8 :: Int

data Registers = Registers { regA :: Int
                           , regB :: Int
                           , regC :: Int
                           , ip :: Int
                           , tx :: String
                           } deriving (Show)

type Program = [(Int, Int)]
type ComputerState = State (Registers, Program) ()

splitOn :: Char -> String -> [String]
splitOn x cs = case dropWhile (x ==) cs of
    ""  -> []
    cs' -> c : splitOn x cs''
        where (c, cs'') = break (x ==) cs'

parse :: String -> ComputerState
parse cs =
    let ls = lines cs
        ra = read $ drop 12 $ head ls
        rb = read $ drop 12 $ ls !! 1
        rc = read $ drop 12 $ ls !! 2
        ns = map read $ splitOn ',' $ drop 9 $ last ls
        chunk [] = []
        chunk (o:n:xs) = (o, n) : chunk xs
    in  state $ const ((), (Registers ra rb rc 0 [], chunk ns))

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

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    let initialState = execState (parse contents) (Registers 0 0 0 0 [], [])
    let pred = (>) instructionCount . ip . fst
    print $ tx $ fst $ last $ takeWhile pred $ iterate step initialState
    putStrLn "Part 2:"
