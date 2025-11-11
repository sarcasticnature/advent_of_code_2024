module Main where

import AoCLib
import System.Environment (getArgs)

pickDay :: String -> (String -> IO ())
pickDay day
  | day == "1" = day01
  | day == "2" = day02
  | day == "3" = day03
  | day == "4" = day04
  | day == "5" = day05
  | day == "6" = day06
  | day == "7" = day07
  | day == "8" = day08
  | day == "9" = day09
  | day == "10" = day10
  | day == "11" = day11
  | day == "14" = day14
  | day == "17" = day17
  | otherwise = \_ -> print "Specified day is unimplemented"

main :: IO ()
main = do
  filename_list <- getArgs
  let day = head filename_list
  let filename = last filename_list
  pickDay day filename
