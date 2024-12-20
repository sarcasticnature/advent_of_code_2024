import System.Environment (getArgs)
import System.IO
import Data.List (foldl')
import Data.Char (digitToInt)
import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST

-- Part 1

toBlocks :: [Int] -> Int -> [Int] -> [Int]
toBlocks acc _ [] = acc
toBlocks acc i [files] = replicate files i ++ acc
toBlocks acc i (files:spaces:ns) = toBlocks acc' (i + 1) ns
    where acc' = replicate spaces (-1) ++ replicate files i ++ acc

defrag :: [Int] -> [Int]
defrag back = reverse $ run [] 0 0
    where forward = reverse back
          run acc i j = let next = forward !! i
                            freeCnt = length $ takeWhile (0 >) $ drop j back
                            (j', acc') = if next >= 0
                                         then (j, next : acc)
                                         else (j + freeCnt + 1, (back !! (j + freeCnt)) : acc)
                        in  if i < (length back - j)
                            then run acc' (i + 1) j'
                            else acc

-- Part 2

data Chunk = File Int Int | Free Int deriving Show

toChunks :: [Chunk] -> Int -> [Int] -> [Chunk]
toChunks acc _ [] = acc
toChunks acc i [fileCnt] = File fileCnt i : acc
toChunks acc i (fileCnt:freeCnt:ns) = toChunks acc' (i + 1) ns
    where acc' = Free freeCnt : File fileCnt i : acc

defrag' :: [Chunk] -> UArray Int Int
defrag' back = reverse $ run start 0 forward
    where forward = reverse back
          arrSize = foldl' (\acc c -> case c of File n _ -> n + acc ; Free n -> n + acc) 0 forward
          start = thaw $ listArray (0, arrSize) (replicate arrSize 0)
          run bs i cs = let next = cs !! i
                            freeCnt = length $ takeWhile (0 >) $ drop j back
                            (j', acc') = if next >= 0
                                         then (j, next : acc)
                                         else (j + freeCnt + 1, (back !! (j + freeCnt)) : acc)
                        in  if i < (length back - j)
                            then run acc' (i + 1) j'
                            else acc

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ sum $ zipWith (*) [0..] $ defrag $ toBlocks [] 0 $ map digitToInt $ init contents
    putStrLn "Part 2:"
    print $ reverse $ toChunks [] 0 $ map digitToInt $ init contents
