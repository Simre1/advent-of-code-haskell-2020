module Day9.Main where

import Data.Foldable ( Foldable(foldl') )
data LastItems a = LastItems Int [a] deriving Show

newLastItems :: Int -> LastItems a
newLastItems i = LastItems i []

addItem :: LastItems a -> a -> LastItems a
addItem (LastItems i l) item = 
   LastItems i $ if length l >= i 
    then item:init l
    else item:l

validNumber :: LastItems Int -> Int -> Bool
validNumber (LastItems i items) n = length items < i || n `elem` [x+y | x <- items, y <- items]

solution1 :: IO ()
solution1 = do
  numbers <- fmap (read @Int) .lines <$>readFile "inputs/day9/input1"
  let lastItems = newLastItems 25
  print $ foldl' (\acc n -> 
    case acc of
      Right a -> Right a
      Left l -> if validNumber l n
          then Left $ addItem l n
          else Right n
    ) (Left lastItems) numbers
  pure ()

solution2 :: IO ()
solution2 = do
  numbers <- fmap (read @Int) .lines <$>readFile "inputs/day9/input1"
  let lastItems = newLastItems 25
  Right invalidNumber <- pure $ foldl' (\acc n -> 
        case acc of
          Right a -> Right a
          Left l -> if validNumber l n
              then Left $ addItem l n
              else Right n
        ) (Left lastItems) numbers
  let solutionList = foldl' (
        let reduce ns n = if  
              | sum ns == invalidNumber -> ns
              | sum ns > invalidNumber -> reduce (init ns) n
              | otherwise -> n:ns
        in reduce ) [] numbers
  print $ minimum solutionList + maximum solutionList
  pure ()