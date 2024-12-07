import System.Environment (getArgs)
import System.IO
import Data.List (foldl')
import qualified Data.ByteString as BS

-- Part 1

-- Part 2

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    let rules = takeWhile (not . null) $ lines contents
    let updates = tail $ dropWhile (not . null) $ lines contents
    putStrLn "Part 1:"
    print $ rules
    print $ updates
    putStrLn "Part 2:"
