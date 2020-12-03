module Day3
  ( part1
  , part2
  , checkForTrees
  )
where

import Input

part1 :: String -> IO Int
part1 filename = checkForTrees 3 1 <$> trees
  where trees = readInputList filename

part2 :: String -> IO Int
part2 filename = do
  a <- (checkForTrees 1 1 <$> trees)
  b <- (checkForTrees 3 1 <$> trees)
  c <- (checkForTrees 5 1 <$> trees)
  d <- (checkForTrees 7 1 <$> trees)
  e <- (checkForTrees 1 2 <$> trees)
  return (a * b * c * d * e)
  where
    trees :: IO [String]
    trees  = readInputList filename

isTree :: Char -> Bool
isTree char = if char == '#' then True else False

checkForTrees :: Int -> Int -> [String] -> Int
checkForTrees  right down trees = checkForTrees' trees 0 0
  where
    l = length $ head trees

    checkForTrees' :: [String] -> Int -> Int -> Int
    checkForTrees' []       _ treeCount = treeCount
    checkForTrees' ts@(t:_) p treeCount
      | isTree (t !! p) = checkForTrees' ts' p' (treeCount + 1)
      | otherwise       = checkForTrees' ts' p' treeCount
      where
        p' = (p + right) `mod` l
        ts' = drop down ts
