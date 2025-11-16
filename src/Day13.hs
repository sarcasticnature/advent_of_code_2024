{-# LANGUAGE TemplateHaskell #-}

module Day13 (day13) where

import Lens.Micro
-- import Lens.Micro.Extras (view)
import Lens.Micro.TH

-- Part 1

data Button = Button {_x :: Int, _y :: Int} deriving (Show)

data Machine = Machine {_a :: Button, _b :: Button, _prize :: Button} deriving (Show)

$(makeLenses ''Button)
$(makeLenses ''Machine)

parseButton :: String -> Button
parseButton cs =
  let cs' = tail $ dropWhile (/= '+') cs
      cs'' = tail $ dropWhile (/= '+') cs'
   in Button (read $ takeWhile (/= ',') cs') (read cs'')

parsePrize :: String -> Button
parsePrize cs =
  let cs' = tail $ dropWhile (/= '=') cs
      cs'' = tail $ dropWhile (/= '=') cs'
   in Button (read $ takeWhile (/= ',') cs') (read cs'')

parse :: String -> [Machine]
parse cs =
  let ls = lines cs
      chunk acc [] = acc
      chunk acc ls' = chunk (m' : acc) ls''
        where
          (m, ls'') = splitAt 4 ls'
          m' = if null $ last m then init m else m
      ms = chunk [] ls
      parseM [as, bs, ps] = Machine (parseButton as) (parseButton bs) (parsePrize ps)
      parseM err = error $ "bad number of lines when parsing machine: " ++ show (length err)
   in map parseM ms

play :: Machine -> Int
play m =
  let d = (m ^. a . x * m ^. b . y) - (m ^. b . x * m ^. a . y)
      d_x = (m ^. prize . x * m ^. b . y) - (m ^. b . x * m ^. prize . y)
      d_y = (m ^. a . x * m ^. prize . y) - (m ^. prize . x * m ^. a . y)
      valid = d_x `mod` d == 0 && d_y `mod` d == 0
      x_ = d_x `quot` d
      y_ = d_y `quot` d
   in if valid then 3 * x_ + y_ else 0

-- Part 2

day13 :: String -> IO ()
day13 filename = do
  contents <- readFile filename
  putStrLn "\nPart 1:"
  print $ sum $ map play $ parse contents
  putStrLn "\nPart 2:"
