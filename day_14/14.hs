import System.Environment (getArgs)
import System.IO
import Data.List (foldl')
import Data.Char (isNumber)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad (zipWithM_)
import Data.Word
import Codec.Picture

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

walk :: (Position, Velocity) -> (Position, Velocity)
walk ((px, py), (vx, vy)) =
    let px' = mod (px + vx) width
        py' = mod (py + vy) height
    in  ((px', py'), (vx, vy))

posToVec :: [Position] -> V.Vector Word8
posToVec ps =
    let empty = do
            v <- MV.new (width * height) :: ST s (V.MVector s Word8)
            MV.set v 0
            return v
        run acc (x, y) = do
            v <- acc
            MV.write v (y * width + x) 255
            return v
        res = foldl' run empty ps
    in  runST $ do v <- res ; V.freeze v

vecToImg :: V.Vector Word8 -> Image Pixel8
vecToImg v =
    let gen x y = v V.! (x + y * width)
    in  generateImage gen width height

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ safetyFactor $ map (simulate . parse) $ lines contents
    putStrLn "Part 2:"
    zipWithM_ writePng ["./images/" ++ show x ++ ".png" | x <- [0..]] $ map (vecToImg . posToVec) $ take 10000 $ map (map fst) $ iterate (map walk) $ map parse $ lines contents
