module Day6.Main where

import Data.Set (size, singleton, intersection, Set(..), fromList)
import Data.List.Split (splitOn)
import Data.Monoid (Sum(Sum))
import Control.Arrow ((<<<))


parseFile :: String -> [Set Char]
parseFile str = foldMap singleton <$> fmap (mconcat . words) (splitOn "\n\n" str)

solution1 :: IO ()
solution1 = readFile"inputs/day6/input1">>=print.foldMap(Sum .size.(foldMap singleton.mconcat.words)).splitOn"\n\n"

solution2 :: IO ()
solution2 = readFile"inputs/day6/input1">>=print.foldMap(Sum .size.(foldr(intersection.foldMap singleton)(fromList['a'..'z']).lines)).splitOn"\n\n"