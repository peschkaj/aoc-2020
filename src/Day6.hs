module Day6 where

import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.List          (nub)
import           Data.List.Split    (splitOn)

combineSurvey :: [String] -> String
combineSurvey xs = nub $ concat xs

part1 :: String -> IO Int
part1 filename =
  do
    s <- readFile filename
    return $ sum $ map length $ map combineSurvey $ map words $ splitOn "\n\n" s


parse :: String -> [[Set Char]]
parse = (map . map) Set.fromList . map lines . splitOn "\n\n"

part2 :: String -> IO Int
part2 filename =
  do
    input <- parse <$> readFile filename
    -- Set.intersection gives the ∩ of the survey answers
    -- then we can measure the size to see who answered in a particular way
    -- finally we sum that to give us the total count
    return . sum . map (Set.size . foldl1 Set.intersection) $ input
