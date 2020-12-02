module Main where

import Lib

main :: IO ()
main = do
  p1 <- part1 "C:\\Users\\jeremiah\\src\\peschkaj\\aoc2020\\day2\\input.txt"
  print p1
  p2 <- part2 "C:\\Users\\jeremiah\\src\\peschkaj\\aoc2020\\day2\\input.txt"
  print p2
