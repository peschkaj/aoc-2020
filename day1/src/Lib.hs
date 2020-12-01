module Lib
    ( readIntCode
    , find2020
    , find2020'
    ) where

readIntCode :: String -> IO [Int]
readIntCode filename = wordsToInts . words <$> readFile filename
  where wordsToInts = map read

sublistofsize :: Int -> [a] -> [[a]]
sublistofsize n xs = sublistofsize' n xs [] []
  where
    sublistofsize' 0 _        prefix otherResults = reverse prefix : otherResults
    sublistofsize' _ []       _      otherResults = otherResults
    sublistofsize' n (x : xs) prefix otherResults = sublistofsize' (n-1) xs (x:prefix) (sublistofsize' n xs prefix otherResults)

combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 1 as        = map pure as
combinationsOf k as@(x:xs) = run (l-1) (k-1) as $ combinationsOf (k-1) xs
                             where
                             l = length as

                             run :: Int -> Int -> [a] -> [[a]] -> [[a]]
                             run n k ys cs | n == k    = map (ys ++) cs
                                           | otherwise = map (q:) cs ++ run (n-1) k qs (drop dc cs)
                                           where
                                           (q:qs) = take (n-k+1) ys
                                           dc     = product [(n-k+1)..(n-1)] `div` product [1..(k-1)]

find2020 :: Int -> [Int] -> [Int]
find2020 n xs = head $ take 1 $ filter eq2020 $ sublistofsize n xs
  where eq2020 p = sum p == 2020

find2020' :: Int -> [Int] -> [Int]
find2020' n xs = head $ take 1 $ filter eq2020 $ combinationsOf n xs
  where eq2020 p = sum p == 2020
