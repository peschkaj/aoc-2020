module Day5 (part1, part2) where

import Data.List (sort, (\\))

binToDec :: Int -> Int
binToDec 0 = 0
binToDec i = 2 * binToDec (div i 10) + mod i 10

-- decToBin :: Int -> Int
-- decToBin 0 = [0]
-- decToBin n = go n
--   where
--     go 0 = []
--     go k = mod k 2 : go (div k 2)

toRow :: String -> Int
toRow xs = binToDec $ read $ toRowString xs
  where
    toRowString []     = ""
    toRowString (x:xs) | x == 'B'  = '1' : toRowString xs
                       | otherwise = '0' : toRowString xs

toSeat :: String -> Int
toSeat xs = binToDec $ read $ toSeatString xs
  where
    toSeatString [] = ""
    toSeatString (x:xs) | x == 'R'  = '1' : toSeatString xs
                        | otherwise = '0' : toSeatString xs

codeToId :: String -> Int
codeToId xs = (row * 8) + seat
  where
    row = toRow $ take 7 xs
    seat = toSeat $ drop 7 xs

findSeat :: Int -> Int -> [Int] -> Int
findSeat low high seats = head $ [low .. high] \\ seats

part1 filename = do
  contents <- readFile filename
  let codes = lines contents
      seats = map codeToId codes
      maxSeat = maximum seats
  print maxSeat

part2 filename = do
  contents <- readFile filename
  let codes = lines contents
      seats = sort $ map codeToId codes
      maxSeat = maximum seats
      minSeat = minimum seats
  print $ findSeat minSeat maxSeat seats
