{-# LANGUAGE ViewPatterns #-}
module Day22.Main where

import Data.List.Split ( splitOn )
import qualified Data.Set as S

parseInput :: String -> ([Int], [Int])
parseInput str = 
  let [p1,p2] = splitOn "\n\n" str
      f = fmap read . drop 1 . lines
  in (f p1, f p2)

solution1 :: IO ()
solution1 = do
  input <- parseInput <$> readFile "inputs/day22/input1"
  print $ case playGame1 input of
    P1Wins hand -> calculateScore hand
    P2Wins hand -> calculateScore hand

solution2 :: IO ()
solution2 = do
  input <- parseInput <$> readFile "inputs/day22/input1"
  print $ case playGame2 S.empty input of
    P1Wins hand -> calculateScore hand
    P2Wins hand -> calculateScore hand

calculateScore :: [Int] -> Int
calculateScore xs = snd $ go xs
  where
    go :: [Int] -> (Int, Int)
    go [] = (1,0)
    go (x:xs) = 
      let (m, s) = go xs
      in (succ m, s + m * x)

data Result = P1Wins [Int] | P2Wins [Int]

playGame1 :: ([Int], [Int]) -> Result
playGame1 (c1:d1,c2:d2) = 
  let (d1', d2') = if c1 > c2 then (d1 ++ [c1,c2], d2) else (d1, d2 ++ [c2,c1])
  in if
    | null d1' -> P2Wins d2'
    | null d2' -> P2Wins d1'
    | otherwise -> playGame1 (d1',d2')

playGame2 :: S.Set ([Int], [Int]) -> ([Int], [Int]) -> Result
playGame2 state ([], d2) = P2Wins d2
playGame2 state (d1, []) = P1Wins d1
playGame2 state (c1:d1, c2:d2)
  | (c1:d1, c2:d2) `S.member` state = P1Wins (c1:d1)
  | c1 > length d1 || c2 > length d2 = playGame2 (S.insert (c1:d1,c2:d2) state) 
      (if c1 > c2 then (d1 ++ [c1,c2], d2) else (d1, d2 ++ [c2,c1]))
  | otherwise = case playGame2 state (take c1 d1, take c2 d2) of
      P1Wins _ -> playGame2 state (d1 ++ [c1,c2], d2)
      P2Wins _ -> playGame2 state (d1, d2 ++ [c2,c1])