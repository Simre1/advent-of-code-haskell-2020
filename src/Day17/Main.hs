module Day17.Main where

import qualified Data.Massiv.Array as A
import Data.Monoid ( Sum(Sum, getSum) )
import Data.Maybe (fromMaybe)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ a = a
applyNTimes i f a = applyNTimes (pred i) f (f a)

parseInput :: String -> ([Int], Int)
parseInput str =
  let l = lines str
   in (fmap (\c -> if c == '#' then 1 else 0) (mconcat l), length l)


solution1 :: IO ()
solution1 = do
  (input, row) <- parseInput <$> readFile "inputs/day17/input1"
  let array = A.resize' (A.Sz3 row row 1) (A.fromList A.Seq input)
  let iteration arr = 
        let biggerArray = A.makeArrayR A.U A.Seq (A.size arr + A.Sz3 2 2 2) (\ix -> fromMaybe 0 $ arr A.!? (A.liftIndex pred ix))
        in A.compute @A.U . A.mapStencil (A.Fill 0) stencil1 $ biggerArray
  let newArray = applyNTimes 6 iteration array
  print $ A.sum newArray

stencil1 :: A.Stencil A.Ix3 Int Int
stencil1 = A.makeStencil (A.Sz3 3 3 3) (A.Ix3 1 1 1) $ \f ->
  let neighbors =
        let offsets = [-1,0,1]
        in mconcat $
          fmap Sum . f
            <$> [A.Ix3 x y z | x <- offsets, y <- offsets, z <- offsets, x /= 0 || y /= 0 || z /= 0]
    in decideCubeState <$> f (A.Ix3 0 0 0) <*> (getSum <$> neighbors)

solution2 :: IO ()
solution2 = do
  (input, row) <- parseInput <$> readFile "inputs/day17/input1"
  let array = A.resize' (A.Sz4 1 1 row row) (A.fromList A.Seq input)
  let iteration arr = 
        let biggerArray = A.makeArrayR A.U A.Seq (A.size arr + A.Sz4 2 2 2 2) (\ix -> fromMaybe 0 $ arr A.!? (A.liftIndex pred ix))
        in A.compute @A.U . A.mapStencil (A.Fill 0) stencil2 $ biggerArray
  let newArray = applyNTimes 6 iteration array
  print $ A.sum newArray

stencil2 :: A.Stencil A.Ix4 Int Int
stencil2 = A.makeStencil (A.Sz4 3 3 3 3) (A.Ix4 1 1 1 1) $ \f -> 
  let neighbors =
        let offsets = [-1,0,1]
        in mconcat $
          fmap Sum . f
            <$> [A.Ix4 x y z w | x <- offsets, y <- offsets, z <- offsets, w <- offsets, x /= 0 || y /= 0 || z /= 0 || w /= 0]
    in decideCubeState <$> f (A.Ix4 0 0 0 0) <*> (getSum <$> neighbors)

decideCubeState :: Int -> Int -> Int
decideCubeState 1 x = if x == 2 || x == 3 then 1 else 0
decideCubeState 0 x = if x == 3 then 1 else 0
decideCubeState x _ = error "Invalid cube state; cube must be 1 or 0"

