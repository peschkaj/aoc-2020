module Lib
    ( checkEntry
    , checkEntries
    , part1
    , part2
    ) where

import Input
import Data.List (foldl')
import Data.Char (isLetter, isDigit)
import Text.ParserCombinators.ReadP ( ReadP
                                    , readP_to_S
                                    , satisfy
                                    , munch1
                                    , eof
                                    , char
                                    , many1)

data Entry    = Entry Policy Password deriving (Show)
data Policy   = Policy Int Int Char deriving (Show)
type Password = String

part1 :: String -> IO String
part1 filename = show <$> withInput filename (readInputListParsed parseEntry) ce
  where ce = checkEntries checkEntry

part2 :: String -> IO String
part2 filename = show <$> withInput filename (readInputListParsed parseEntry) ce
  where ce = checkEntries checkEntry'

checkEntries :: (Entry -> Bool) -> [Entry] -> Int
checkEntries f es = foldl' (\acc item -> if item then acc + 1 else acc) 0 $ map f es

checkEntry :: Entry -> Bool
checkEntry (Entry (Policy minCount maxCount letter) password) = lc >= minCount && lc <= maxCount
  where lc = occurences letter password

checkEntry' :: Entry -> Bool
checkEntry' (Entry (Policy p1 p2 letter) password) = (first == letter) /= (second == letter)
  where first = password !! (p1 - 1)
        second = password !! (p2 - 1)

occurences :: Char -> Password -> Int
occurences letter = foldl' (\acc item -> if item == letter then acc + 1 else acc) 0

positiveInt :: (Integral i, Read i) => ReadP i
positiveInt = read <$> many1 (satisfy isDigit)

parseEntry :: String -> Entry
parseEntry = parseBest entryParser

parseBest :: ReadP a -> String -> a
parseBest parser = fst . head . readP_to_S (parser <* eof)

entryParser :: ReadP Entry
entryParser = do
  minCount <- positiveInt
  _ <- char '-'
  maxCount <- positiveInt
  _ <- char ' '
  letter <- satisfy isLetter
  _ <- char ':' *> char ' '
  password <- munch1 isLetter
  return $ Entry (Policy minCount maxCount letter) password
