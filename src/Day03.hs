module Day03 (day03) where

import Data.List (foldl')
import Data.Char (isNumber)
import Text.Read (readMaybe)

-- Part 1

readMul :: String -> Either Int (Int, Int)
readMul cs =
    let mulTaken = length $ takeWhile id $ zipWith (==) "mul(" cs
        mulDrop cnt
            | cnt == 4 = Right 0
            | cnt == 0 = Left 1
            | otherwise = Left cnt
        mul = mulDrop mulTaken
        firstStr = takeWhile isNumber $ drop 4 cs
        firstNum = readMaybe firstStr :: Maybe Int
        first = case firstNum of Just n -> Right n ; Nothing -> Left (4 + length firstStr)
        commaBool = (==) "," $ take 1 $ drop (4 + length firstStr) cs
        comma = if commaBool then Right 0 else Left $ 5 + length firstStr
        secondStr = takeWhile isNumber $ drop (5 + length firstStr) cs
        secondNum = readMaybe secondStr :: Maybe Int
        second = case secondNum of
            Just n -> Right n
            Nothing -> Left $ 5 + length firstStr + length secondStr
        parenBool = (==) ")" $ take 1 $ drop (5 + length firstStr + length secondStr) cs
        paren = if parenBool then Right 0 else Left (6 + length firstStr + length secondStr)
        either = do
            mul
            a <- first
            comma
            b <- second
            paren
            return $ a * b
    in  case either of
            Right n -> Right (n, 6 + length firstStr + length secondStr)
            Left n  -> Left n

parseMul :: Int -> String -> Int
parseMul acc "" = acc
parseMul acc cs = case readMul cs of
    Right (n, cnt) -> parseMul (acc + n) $ drop cnt cs
    Left cnt  -> parseMul acc $ drop cnt cs

-- Part 2

readDo :: String -> Maybe Bool
readDo cs =
    let doFound = (==) 4 $ length $ takeWhile id $ zipWith (==) "do()" cs
        dontFound = (==) 7 $ length $ takeWhile id $ zipWith (==) "don't()" cs
    in  if doFound || dontFound then Just doFound else Nothing

parseMulDo :: Bool -> Int -> String -> Int
parseMulDo _ acc "" = acc
parseMulDo b acc cs = case (readDo cs, readMul cs) of
    (Just b', _)        -> parseMulDo b' acc $ if b' then drop 4 cs else drop 7 cs
    (_, Right (n, cnt)) -> if b
                           then parseMulDo b (acc + n) $ drop cnt cs
                           else parseMulDo b acc $ drop cnt cs
    (Nothing, Left cnt)  -> parseMulDo b acc $ drop cnt cs

day03 :: String -> IO ()
day03 filename = do
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ parseMul 0 contents
    putStrLn "Part 2:"
    print $ parseMulDo True 0 contents
