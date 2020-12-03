module Day3.Main where

import qualified Data.Map as M

-- . -> True, # -> False
parseInput :: String -> M.Map (Int, Int) Bool
parseInput str = go (0,0) str M.empty
  where 
    go (x,y) = \case
      "" -> id
      ('\n':c) -> go (0,succ y) c
      (c:cs) -> go (succ x, y) cs . M.insert (x,y) (c == '.')

testSlope :: M.Map (Int, Int) Bool -> ((Int, Int) -> (Int, Int)) -> Int
testSlope treeMap slope = do
  let (largestX, largestY) = M.foldrWithKey (\k1 _ k2 -> if k1 > k2 then k1 else k2) (0,0) treeMap
      countTree (x,y) = if treeMap M.! (x `mod` succ largestX, y) then id else succ @Int
      traversePath pos@(_,y) = 
        if y > largestY
          then 0
          else 
            let newPos = slope pos
            in countTree pos $ traversePath newPos
  traversePath (0,0)

makeSlope :: (Int -> Int) -> (Int -> Int) -> (Int, Int) -> (Int, Int)
makeSlope fX fY (x,y) = (fX x, fY y)

solution1 :: IO ()
solution1 = do
  treeMap <- parseInput <$> readFile "inputs/day3/input1"
  print $ testSlope treeMap $ makeSlope (+3) (+1)

solution2 :: IO ()
solution2 = do
  treeMap <- parseInput <$> readFile "inputs/day3/input1"
  print $ product $ testSlope treeMap <$> 
    [ makeSlope (+1) (+1), 
      makeSlope (+3) (+1), 
      makeSlope (+5) (+1),
      makeSlope (+7) (+1),
      makeSlope (+1) (+2)
    ]