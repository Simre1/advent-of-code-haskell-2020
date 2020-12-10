module Day10.Main where
import Data.List (foldl', sort)

solution1 :: IO ()
solution1 = do
  adapters <- sort . fmap (read @Int) . words <$> readFile "inputs/day10/input1"
  let loop [_] = [3]
      loop (x:y:xs) = (y - x):loop (y:xs)
      differences = loop (0:adapters)
  print $ length (filter (==1) differences) * length (filter (==3) differences)

solution2 :: IO ()
solution2 = do
  adapters <- sort . fmap (read @Int) . words <$> readFile "inputs/day10/input1"
  let loop [_] = [3]
      loop (x:y:xs) = (y - x):loop (y:xs)
      differences = loop (0:adapters)
  let 
    permutations n = tribonacci (n+2)
    solution = snd $ foldl' (\(ones, c) d -> case d of
      1 -> (succ ones, c)
      3 -> (0, permutations ones * c)
      ) (0,1) differences
  print solution
  pure ()

tribonacci :: (Eq a, Num a, Num p) => a -> p
tribonacci 0 = 0
tribonacci 1 = 0
tribonacci 2 = 1
tribonacci n = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)

-- 1 -> 1
-- 1+1 -> 1+1, 2
-- 1+1+1 -> 3,1+1+1,2+1,1+2
-- 1+1+1+1 -> 3+1,1+3,1+1+1+1,1+1+2,1+2+1,2+1+1,2+2
-- 1+1+1+1+1 -> 3+2,2+3,3+1+1,1+1+3,1+3+1,2+2+1,2+1+2,1+2+2,1+1+1+2,1+1+2+1,1+2+1+1,2+1+1+1,1+1+1+1+1
-- oeis.org -> tribonacci-numbers
