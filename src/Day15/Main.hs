module Day15.Main where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Foldable (Foldable(foldl'))
import qualified Data.Array.MArray as A
import qualified Data.Array.IO as A

import Control.DeepSeq ( force )

solution1 :: IO ()
solution1 = do
  numbers <- fmap (read @Int) . splitOn "," <$> readFile "inputs/day15/input1"
  let numberMap = HM.fromList $ zip numbers (repeat 0 `zip` [1..])
  let (lastNumber, _) = foldl' speakNumber (last numbers, numberMap) [length numbers+1..2020]
  print lastNumber

speakNumber :: (Int, HM.HashMap Int (Int,Int)) -> Int -> (Int, HM.HashMap Int (Int,Int))
speakNumber (last, numbers) turn = 
  let (firstTurn,secondTurn) = numbers HM.! last
      (newNumber) = if firstTurn == 0 then 0 else (secondTurn - firstTurn)
      newMap = HM.alter (pure . maybe (0,turn) (\(_,b) -> (b,turn))) newNumber numbers
  in force $ (newNumber, newMap)

speakNumberMut :: A.IOArray Int (Int, Int) -> Int -> Int -> IO Int
speakNumberMut table turn last = do
  (firstTurn,secondTurn) <- A.readArray table last
  let newNumber = if firstTurn == 0 then 0 else secondTurn - firstTurn
  (_,y) <- A.readArray table newNumber
  A.writeArray table newNumber (y,turn)
  pure newNumber

solution2 :: IO ()
solution2 = do
  numbers <- fmap (read @Int) . splitOn "," <$> readFile "inputs/day15/input1"
  numberTable <- A.newArray (0,30000000) (0,0)
  foldMap (uncurry (A.writeArray numberTable)) (zip numbers (repeat 0 `zip` [1..]))
  lastNumber <- foldl' (\b a -> b >>= speakNumberMut numberTable a) (pure $ last numbers) [length numbers+1..30000000]
  print lastNumber