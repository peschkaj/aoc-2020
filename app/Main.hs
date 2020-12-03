module Main where

import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3

main :: IO ()
main = do
  {-
  -- day 1
  xs <- readIntCode "C:\\Users\\jeremiah\\src\\peschkaj\\aoc2020\\day1\\input.txt"
  let p = find2020 2 xs
    in print $ product p
  let p = find2020 3 xs
    in print $ product p
    -}

  -- day 2
  p1 <- Day2.part1 "C:\\Users\\jeremiah\\src\\peschkaj\\aoc2020\\day2.txt"
  print p1
  p2 <- Day2.part2 "C:\\Users\\jeremiah\\src\\peschkaj\\aoc2020\\day2.txt"
  print p2
