import System.Environment (getArgs)
import System.IO
import Data.Char (digitToInt)

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

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ sum $ zipWith (*) [0..] $ defrag $ toBlocks [] 0 $ map digitToInt $ init contents
    putStrLn "Part 2:"
