import System.Environment (getArgs)
import System.IO
import Data.List (sort, foldl')
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import Criterion.Main
import Criterion.Main.Options

parseLine :: BS.ByteString -> (Int, Int)
parseLine cs = (l, r)
    where ws = BC.words cs
          Just (l, _) = BC.readInt $ head ws
          Just (r, _) = BC.readInt $ last ws

folder (rs, acc) l = let cnt = length $ takeWhile (l ==) $ dropWhile (l >) rs
                         rs' = dropWhile (l >) rs
                     in  (rs', acc + l * cnt)

similarity :: ([Int], [Int]) -> Int
similarity (ls, rs) = snd $ foldl' folder (rs, 0) ls

myMain filename = do
    contents <- BS.readFile filename
    let sorted = bimap sort sort $ unzip $ map parseLine $ BC.lines contents
    --putStrLn "Part 1:"
    let foo = sum $ uncurry (zipWith (\x y -> abs $ x - y)) sorted
    --putStrLn "Part 2:"
    let bar = similarity sorted
    return (foo,bar)

main = defaultMain [
        bench "foo" $ nfIO $ myMain "data.txt"
    ]
