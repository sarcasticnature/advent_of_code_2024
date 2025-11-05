module Day04 (day04) where

import Data.List (intersect, transpose)

-- Part 1

diagonalIdx :: Int -> Int -> [[(Int, Int)]]
diagonalIdx height width =
  let stopRow = height - 4 -- hardcoded for a 4 length search str
      stopCol = width - 4
      runIdx x y = zip [x .. width - 1] [y .. height - 1]
      rightUpper = map (`runIdx` 0) [0 .. stopCol]
      leftLower = tail $ map (runIdx 0) [0 .. stopRow]
      runIdx' x y = zip [x .. width - 1] [height - 1 - y, height - 2 - y .. 0]
      rightLower = map (`runIdx'` 0) [0 .. stopCol]
      leftUpper = tail $ map (runIdx' 0) [0 .. stopRow]
   in rightUpper ++ leftLower ++ rightLower ++ leftUpper

idxToList :: [String] -> [(Int, Int)] -> String
idxToList puzzle idxs =
  let idxToChar (x, y) = (puzzle !! y) !! x
   in map idxToChar idxs

combos :: String -> [String]
combos cs =
  let horizontal = lines cs
      vertical = transpose horizontal
      height = length horizontal
      width = length $ head horizontal
      idxs = diagonalIdx height width
      diags = map (idxToList horizontal) idxs
   in horizontal ++ vertical ++ diags

readXmas :: String -> Bool
readXmas cs =
  let xmasFound = (==) 4 $ length $ takeWhile id $ zipWith (==) "XMAS" cs
      samxFound = (==) 4 $ length $ takeWhile id $ zipWith (==) "SAMX" cs
   in xmasFound || samxFound

parseXmas :: Int -> String -> Int
parseXmas acc "" = acc
parseXmas acc cs =
  if readXmas cs
    then parseXmas (acc + 1) $ drop 3 cs
    else parseXmas acc $ drop 1 cs

-- Part 2

diagonalIdx' :: Int -> Int -> ([[(Int, Int)]], [[(Int, Int)]])
diagonalIdx' height width =
  let stopRow = height - 3 -- hardcoded for a 3 length search str
      stopCol = width - 3
      runIdx x y = zip [x .. width - 1] [y .. height - 1]
      rightUpper = map (`runIdx` 0) [0 .. stopCol]
      leftLower = tail $ map (runIdx 0) [0 .. stopRow]
      runIdx' x y = zip [x .. width - 1] [height - 1 - y, height - 2 - y .. 0]
      rightLower = map (`runIdx'` 0) [0 .. stopCol]
      leftUpper = tail $ map (runIdx' 0) [0 .. stopRow]
   in (rightUpper ++ leftLower, rightLower ++ leftUpper)

idxToList' :: [String] -> [(Int, Int)] -> String
idxToList' puzzle idxs =
  let idxToChar (x, y) = (puzzle !! y) !! x
   in map idxToChar idxs

readMas :: String -> Bool
readMas cs =
  let xmasFound = (==) 3 $ length $ takeWhile id $ zipWith (==) "MAS" cs
      samxFound = (==) 3 $ length $ takeWhile id $ zipWith (==) "SAM" cs
   in xmasFound || samxFound

collectMas :: [Bool] -> String -> [Bool]
collectMas acc "" = acc
collectMas acc cs = collectMas (acc ++ [readMas cs]) $ drop 1 cs

findCross :: [Bool] -> [Bool] -> Int
findCross a b = sum $ map fromEnum $ zipWith (&&) a b

countMas :: String -> Int
countMas cs =
  let horizontal = lines cs
      height = length horizontal
      width = length $ head horizontal
      (crissIdx, crossIdx) = diagonalIdx' height width
      crissStr = map (idxToList' horizontal) crissIdx
      crossStr = map (idxToList' horizontal) crossIdx
      crissBool = map (init . (False :) . collectMas []) crissStr
      crossBool = map (init . (False :) . collectMas []) crossStr
      crissGood = map snd $ concatMap (filter fst) $ zipWith zip crissBool crissIdx
      crossGood = map snd $ concatMap (filter fst) $ zipWith zip crossBool crossIdx
   in length $ intersect crissGood crossGood

day04 :: String -> IO ()
day04 filename = do
  contents <- readFile filename
  putStrLn "Part 1:"
  print $ sum $ map (parseXmas 0) $ combos contents
  putStrLn "Part 2:"
  print $ countMas contents
