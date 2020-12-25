module Day25.Main where

parseInput :: String -> (Int, Int)
parseInput str = let [k1,k2] = lines str in (read k1, read k2)

solution1 :: IO ()
solution1 = do
  (publicK1,publicK2) <- parseInput <$> readFile "inputs/day25/input1"
  let (l1,l2) = (findLoopSize publicK1, findLoopSize publicK2)
      (privateK1, privateK2) = (applyNTimes l2 (transformKey publicK1) 1, applyNTimes l1 (transformKey publicK2) 1)
  print (privateK1, privateK2)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ !a = a
applyNTimes x f !a = applyNTimes (pred x) f $ f a

subjectNumber :: Integer
subjectNumber = 7

transformKey :: Integral a => a -> a -> a
transformKey subject x = (x * subject) `mod` 20201227

findLoopSize :: Integral a => a -> Int
findLoopSize public = 
  let (p1,p2) = break (== public) $ iterate (transformKey initialSubjectNumber) 1
  in length p1
  where initialSubjectNumber = 7

solution2 :: IO ()
solution2 = pure ()