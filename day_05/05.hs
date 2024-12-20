import System.Environment (getArgs)
import System.IO
import Data.List (foldl')
import Control.Monad
import qualified Data.HashMap.Strict as HM

-- Part 1

splitOn :: Char -> String -> [String]
splitOn x cs = case dropWhile (x ==) cs of
    ""  -> []
    cs' -> c : splitOn x cs''
        where (c, cs'') = break (x ==) cs'

parseRule :: String -> (Int, Int)
parseRule cs =
    let a = read $ takeWhile ('|' /=) cs
        b = read $ drop 1 $ dropWhile ('|' /=) cs
    in  (a, b)

parseUpdate :: String -> [Int]
parseUpdate cs = map read $ splitOn ',' cs

collectRules :: [(Int, Int)] -> HM.HashMap Int [Int]
collectRules = foldl' run HM.empty
    where run acc (a, b) = HM.insertWith (++) a [b] acc

mid :: [Int] -> Maybe Bool -> Int
mid ns m = case m of
    Nothing    -> 0
    Just False -> 0
    Just True  -> ns !! i
    where i = length ns `div` 2

--valid :: HM.HashMap Int [Int] -> [Int] -> Bool
valid hm ns =
    let zs = zip ns $ tail ns
        run acc (a, b) = (&&) acc .elem b <$> (hm HM.!? a)
    in  foldM run True zs

-- Part 2

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    let rules = collectRules $ map parseRule $ takeWhile (not . null) $ lines contents
    let updates = map parseUpdate $ tail $ dropWhile (not . null) $ lines contents
    putStrLn "Part 1:"
    --print rules
    print $ map (valid rules) updates
    --print $ zipWith mid updates $ map (valid rules) updates
    putStrLn "Part 2:"
