import System.Environment (getArgs)
import System.IO
import Data.List (foldl')
import Data.Char (isNumber)

-- Part 1

type Position = (Int, Int)
type Velocity = (Int, Int)

-- NOTE: width and height will need to be manually updated for test input vs actual
-- test: 11 wide, 7 tall
-- actual: 101 wide, 103 tall
--width = 11 :: Int
--height = 7 :: Int
width = 101 :: Int
height = 103 :: Int

parse :: String -> (Position, Velocity)
parse cs =
    let px = read $ takeWhile isNumber $ drop 2 cs
        py = read $ takeWhile isNumber $ drop 1 $ dropWhile (',' /=) cs
        vel = words cs !! 1
        vx = read $ takeWhile (',' /=) $ drop 2 vel
        vy = read $ drop 1 $ dropWhile (',' /=) vel
    in  ((px, py), (vx ,vy))

simulate :: (Position, Velocity) -> Position
simulate ((px, py), (vx, vy)) =
    let px' = mod (px + vx * 100) width
        py' = mod (py + vy * 100) height
    in  (px', py')

safetyFactor :: [Position] -> Int
safetyFactor ps =
    let run (a, b, c, d) (px, py)
            | px > div width 2 && py < div height 2  = (a + 1, b, c, d)
            | px < div width 2 && py < div height 2  = (a, b + 1, c, d)
            | px < div width 2 && py > div height 2  = (a, b, c + 1, d)
            | px > div width 2 && py > div height 2  = (a, b, c, d + 1)
            | otherwise                              = (a, b, c, d)
        (q1, q2, q3, q4) = foldl' run (0, 0, 0, 0) ps
    in  q1 * q2 * q3 * q4

-- Part 2

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ safetyFactor $ map (simulate . parse) $ lines contents
    putStrLn "Part 2:"
