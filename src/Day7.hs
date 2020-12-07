{-# LANGUAGE ViewPatterns #-}
module Day7 where

import Data.List (unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)

parse :: String -> Map (String, String) [(Int, (String, String))]
parse = Map.fromList . mapMaybe (parseLine . words) . lines
  where
    parseLine (a:b:_:_:rest) = Just ((a,b), unfoldr parseItems rest)
    parseLine _ = Nothing
    parseItems ((reads -> (n, ""):_):a:b:_:rest) = Just ((n :: Int, (a, b)), rest)
    parseItems _ = Nothing

part1 :: String -> Int
part1 input = Map.size $ Map.filter (any hasGold) bags
  where
    bags = parse input
    -- reminder that !? finds the value at a key OR returns Nothing
    hasGold (_, ("shiny", "gold")) = True
    hasGold (_, item) = maybe False (any hasGold) $ bags Map.!? item

part2 :: String -> Int
part2 input = sum $ countBags 1 ("shiny", "gold")
  where
    bags = parse input
    countBags n item = do
      (m, item') <- fromMaybe [] $ bags Map.!? item
      let n' = n * m
      n' : countBags n' item'
