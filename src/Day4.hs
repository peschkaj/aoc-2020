module Day4 where

import Data.Char (isHexDigit, isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split
import Control.Arrow (second)
import Text.Read (readMaybe)

type Passport = Map String String

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

splitOnOne :: Char -> String -> (String, String)
splitOnOne c = second (drop 1) . break (== c)

mkPassport :: [String] -> Passport
mkPassport = Map.fromList . map (splitOnOne ':')

numBetween :: Int -> Int -> String -> Bool
numBetween l r s = maybe False (\n -> l <= n && n <= r) (readMaybe s)

validHeight :: String -> Bool
validHeight (a:b:c:"cm") = numBetween 150 193 [a,b,c]
validHeight (a:b:"in") = numBetween 59 76 [a, b]
validHeight _ = False

validHairColor :: String -> Bool
validHairColor color =
  length color == 7
  && head color == '#'
  && all isHexDigit (tail color)

validEyeColor :: String -> Bool
validEyeColor eye = elem eye validEyes
  where
    validEyes = [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

validPassportID :: String -> Bool
validPassportID pid = length pid == 9 && all isDigit pid

validPassport :: Map String (String -> Bool)
validPassport = Map.fromList [
    ("byr", numBetween 1920 2002)
  , ("iyr", numBetween 2010 2020)
  , ("eyr", numBetween 2020 2030)
  , ("hgt", validHeight)
  , ("hcl", validHairColor)
  , ("ecl", validEyeColor)
  , ("pid", validPassportID) ]

checkPassport1 :: Passport -> Bool
checkPassport1 p =
  Map.member "byr" p &&
  Map.member "iyr" p &&
  Map.member "eyr" p &&
  Map.member "hgt" p &&
  Map.member "hcl" p &&
  Map.member "ecl" p &&
  Map.member "pid" p

checkPassport2 :: Passport -> Bool
checkPassport2 p =
  checkPassport1 p &&
  and (Map.elems (Map.intersectionWith ($) validPassport p))

part2 :: String -> IO Int
part2 filename =
  do
    s <- readFile filename
    let pps = map words $ splitOn "\n\n" s
        passports = map mkPassport pps
    return $ length $ filter checkPassport2 passports

part1 :: String -> IO Int
part1 filename =
  do
    s <- readFile filename
    let pps = map words $ splitOn "\n\n" s
        passports = map mkPassport pps
    return $ length $ filter checkPassport1 passports
