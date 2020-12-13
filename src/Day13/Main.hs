module Day13.Main where

import Data.List.Split (splitOn)
import Control.Arrow (second)

solution1 :: IO ()
solution1 = do
  (a : b : _) <- lines <$> readFile "inputs/day13/input1"
  let (time, bus) = head . dropWhile ((read a >) . fst ) . (\l -> [(x * i,x) | i <- [1 ..], x <- l]) . 
        fmap (read @Int) . filter (/= "x") . splitOn "," $ b
  print $ (time - read a) * bus

solution2 :: IO ()
solution2 = do
  (_ : b : _) <- lines <$> readFile "inputs/day13/input1"
  let busses = fmap (second $ read @Int) . filter ((/= "x") . snd) $ zip [0..] . splitOn "," $ b
      m = product $ snd <$> busses
      es = fmap 
        (\x -> 
          let m' = m `quot` x
              (y,_,_) = extGCD m' x 
          in y * m'
        ) $ snd <$> busses
      solution = sum (zipWith (*) ((\(bid,bt) -> bt - bid) <$> busses) es) `mod` m
  print solution

extGCD :: Int -> Int -> (Int, Int, Int)
extGCD a 0 = (1, 0, a)
extGCD a b = let (q, r) = a `quotRem` b
                 (s, t, g) = extGCD b r
             in  (t, s - q * t, abs g)