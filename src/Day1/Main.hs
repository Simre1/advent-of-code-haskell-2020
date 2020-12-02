module Day1.Main where


import Criterion.Main
solution1 :: IO ()
solution1 = do
  input <- fmap (read @Int) . lines <$> readFile "inputs/day1/input1"
  let results = do 
        x <- input
        y <- input
        if (x + y == 2020)
          then [(x,y)]
          else []
  print $ let (a,b) = head results in a * b

solution2 :: IO ()
solution2 = do
  input <- fmap (read @Int) . lines <$> readFile "inputs/day1/input1"
  let results = do 
        x <- input
        y <- input
        z <- input
        if (x + y + z == 2020)
          then [(x,y,z)]
          else []
  print $ let (a,b,c) = head results in a * b * c 

shortSolution :: IO ()
shortSolution = readFile"inputs/day1/input1">>=(\x->print$head[a*b|a<-x,b<-x,a+b==2020]).fmap read.lines