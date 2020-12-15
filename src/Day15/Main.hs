module Day15.Main where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Foldable (Foldable(foldl'))
import qualified Data.Array.MArray as A
import Data.Word
import qualified Data.Array.IO as A
import Data.Bits

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

speakNumberMut :: A.IOUArray Word32 Word64 -> Word32 -> Word32 -> IO Word32
speakNumberMut table turn last = do
  (firstTurn,secondTurn) <- word64To32 <$> A.readArray table last
  let newNumber = if firstTurn == 0 then 0 else secondTurn - firstTurn
  (_,y) <- word64To32 <$> A.readArray table newNumber
  A.writeArray table newNumber $ word32To64 y turn
  pure newNumber

solution2 :: IO ()
solution2 = do
  numbers <- fmap (read @Word32) . splitOn "," <$> readFile "inputs/day15/input1"
  numberTable <- A.newArray (0,30000000) (0 :: Word64)
  foldMap (uncurry (A.writeArray numberTable)) (zip numbers (uncurry word32To64 <$> repeat 0 `zip` [1..]))
  lastNumber <- foldl' (\b a -> b >>= speakNumberMut numberTable a) (pure $ last numbers) [fromIntegral (length numbers+1)..30000000]
  print lastNumber

word32To64 :: Word32 -> Word32 -> Word64
word32To64 w1 w2 = shiftL (fromIntegral w1) 32 .|. fromIntegral w2

word64To32 :: Word64 -> (Word32, Word32)
word64To32 w = (fromIntegral $ shiftR w 32,fromIntegral w)