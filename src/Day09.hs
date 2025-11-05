module Day09 (day09) where

import Data.Char (digitToInt)
import Data.List (elemIndex, foldl')

-- Part 1

toBlocks :: [Int] -> Int -> [Int] -> [Int]
toBlocks acc _ [] = acc
toBlocks acc i [files] = replicate files i ++ acc
toBlocks acc i (files : spaces : ns) = toBlocks acc' (i + 1) ns
  where
    acc' = replicate spaces (-1) ++ replicate files i ++ acc

defrag :: [Int] -> [Int]
defrag back = reverse $ run [] 0 0
  where
    forward = reverse back
    run acc i j =
      let next = forward !! i
          freeCnt = length $ takeWhile (0 >) $ drop j back
          (j', acc') =
            if next >= 0
              then (j, next : acc)
              else (j + freeCnt + 1, (back !! (j + freeCnt)) : acc)
       in if i < (length back - j)
            then run acc' (i + 1) j'
            else acc

-- Part 2

data Chunk = File Int Int | Free Int deriving (Show, Eq)

toChunks :: [Chunk] -> Int -> [Int] -> [Chunk]
toChunks acc _ [] = acc
toChunks acc i [fileCnt] = File fileCnt i : acc
toChunks acc i (fileCnt : freeCnt : ns) = toChunks acc' (i + 1) ns
  where
    acc' = Free freeCnt : File fileCnt i : acc

freeFile :: Int -> Int -> [Chunk] -> [Chunk]
freeFile idx cnt cs =
  let (l, r) = splitAt (idx + 1) cs
      l' = init l
   in l' ++ [Free cnt] ++ r

insertFile :: Int -> Chunk -> [Chunk] -> [Chunk]
insertFile i c@(File cnt n) cs =
  let (l, r) = splitAt (i + 1) cs
      l' = init l
      Free fcnt = last l
      free = [Free (fcnt - cnt) | (fcnt - cnt) > 0]
   in l' ++ [c] ++ free ++ r

move :: Chunk -> [Chunk] -> [Chunk]
move f@(Free _) cs = cs
move f@(File cnt n) cs =
  let run (b, cs') (idx, c) = case c of
        (File _ _) -> (b, cs')
        (Free fcnt) ->
          if b && fcnt >= cnt && idx < existingIdx
            then (False, newCs)
            else (b, cs')
        where
          (Just existingIdx) = elemIndex f cs
          newCs = insertFile idx f $ freeFile existingIdx cnt cs
   in snd $ foldl' run (True, cs) $ zip [0 ..] cs

defrag' :: [Chunk] -> [Chunk]
defrag' cs = foldr move cs cs

convert :: [Chunk] -> [Int]
convert = concatMap toInts
  where
    toInts c = case c of
      (File cnt n) -> replicate cnt n
      (Free cnt) -> replicate cnt 0

day09 :: String -> IO ()
day09 filename = do
  contents <- readFile filename
  putStrLn "Part 1:"
  print $ sum $ zipWith (*) [0 ..] $ defrag $ toBlocks [] 0 $ map digitToInt $ init contents
  putStrLn "Part 2:"
  print $ sum $ zipWith (*) [0 ..] $ convert $ defrag' $ reverse $ toChunks [] 0 $ map digitToInt $ init contents
