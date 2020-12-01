module Lib
    ( readIntCode
    , find2020
    ) where

readIntCode :: String -> IO [Int]
readIntCode filename = wordsToInts . words <$> readFile filename
  where wordsToInts = map read

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = if n > l then []
                                   else subsequencesBySize xs !! (l - n)
  where
    l = length xs
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                                in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

find2020 :: Int -> [Int] -> [Int]
find2020 n xs = head $ take 1 $ filter eq2020 $ subsequencesOfSize n xs
  where eq2020 p = sum p == 2020
