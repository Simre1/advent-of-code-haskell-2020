module Day6.Main where

import Data.Set (size, singleton, intersection, Set(..), fromList)
import Data.List.Split (splitOn)
import Data.Monoid (Sum(Sum))

parseFile :: String -> [Set Char]
parseFile str = foldMap singleton <$> fmap (mconcat . words) (splitOn "\n\n" str)

solution1 :: IO ()
solution1 = readFile"inputs/day6/input1">>=print.foldMap(Sum .size.(fromList.mconcat.words)).splitOn"\n\n"

solution2 :: IO ()
solution2 = readFile"inputs/day6/input1">>=print.foldMap(Sum .size.(foldl1 intersection.fmap fromList.lines)).splitOn"\n\n"