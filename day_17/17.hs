import System.Environment (getArgs)
import System.IO
--import Data.List (foldl')
import Control.Monad.State
import Data.Bits (xor)

-- Part 1

-- TODO: hardcoding instruction count
instructionCount = 3 :: Int
--instructionCount = 8 :: Int

data Registers = Registers { regA :: Int, regB :: Int, regC :: Int, ip :: Int, tx :: String }
    deriving (Show)
type Program = [(Int, Int)]
type ComputerState = State (Registers, Program) (Int, String)

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
    in  state $ const ((0, ""), (Registers ra rb rc 0 [], chunk ns))

execute :: (Registers, Program) -> (Registers, Program)
execute (Registers a b c ip tx, is)
    | op == 0 = (Registers dv b c (ip + 2) tx, is)
    | op == 1 = (Registers a bxl c (ip + 2) tx, is)
    | op == 2 = (Registers a bst c (ip + 2) tx, is)
    | op == 3 = (Registers a b c jnz tx, is)
    | op == 4 = (Registers a bxc c (ip + 2) tx, is)
    | op == 5 = (Registers a b c (ip + 2) out, is)
    | op == 6 = (Registers a dv c (ip + 2) tx, is)
    | op == 7 = (Registers a b dv (ip + 2) tx, is)
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
        jnz = if a /= 0 then dat else ip
        bxc = b `xor` c
        outDat = show $ combo dat `mod` 8
        out = if null tx then outDat else tx ++ (',' : outDat)

--run :: (Registers, Program) -> ComputerState -> String
--run rp cs =
--    let ((ip, out), cs') = runState cs rp
--    in  if ip >= instructionCount then out else run cs' (execute cs')

-- Part 2

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ runState (parse contents) (Registers 0 0 0 0 [], [])
    --print $ run (Registers 0 0 0 0 [], []) $ parse contents
    putStrLn "Part 2:"
