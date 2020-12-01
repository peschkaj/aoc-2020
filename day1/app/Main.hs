module Main where

import Lib

main :: IO ()
main = do
    xs <- readIntCode "C:\\Users\\jeremiah\\src\\peschkaj\\aoc2020\\day1\\input.txt"
    let p = find2020 2 xs
      in print $ product p
    let p = find2020 3 xs
      in print $ product p
