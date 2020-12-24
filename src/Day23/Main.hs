{-# LANGUAGE ViewPatterns #-}
module Day23.Main where

import Data.List (sort, (\\),  foldl' )
import Debug.Trace (traceShow, traceShowId)

import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad.Primitive
import Control.Monad.ST ()
import System.Random
import qualified Data.Map.Strict as M
import Control.Monad (forM_)

-- Part 1 with linked lists
executeMoves :: Int -> [Int] -> [Int]
executeMoves i ints = applyNTimes i doMove ints
  where
    doMove :: [Int] -> [Int]
    doMove (c:p1:p2:p3:r) = insertAtPosition (c-1)
      where 
        insertAtPosition a
          | a < lowest = insertAtPosition highest
          | a `elem` (p1:p2:p3:[]) = 
            insertAtPosition (a-1)
          | otherwise =
            let (x1,(x:x2)) = break (==a) r
            in x1 ++ (x:p1:p2:p3:x2) ++ [c]
    lowest :: Int
    lowest = minimum ints
    highest :: Int
    highest = maximum ints

extractSolution1 :: [Int] -> String
extractSolution1 ints = 
  let (p1,_:p2) = break (==1) ints
  in toEnum . (+48) <$> p2 ++ p1

-- Part 2 with mutable array
executeMovesP2 :: Int -> [Int] -> IO Integer
executeMovesP2 i ints = do
  let l = length ints
  cups <- V.new (l+1)
  forM_ (zip (0:ints) (ints ++ [head ints])) $ \(i,e) -> V.write cups i e
  foldl' (\a i -> a >>= go cups) (pure (head ints)) [1..i]
  x <- V.read cups 1
  y <- V.read cups x
  print (x,y)
  pure $ fromIntegral x * fromIntegral y
  
  where
    -- prints the vector for debugging purposes 
    printMem cups = foldl (\o a -> (:) <$> V.read cups a <*> o) (pure []) [V.length cups-1, V.length cups-2..0] >>= print
    printMem2 cups = print =<< do
      x <- V.read cups 7
      let loop a = do
            if a == 7
              then pure []
              else do
                x <- V.read cups a
                rest <- loop x
                pure $ x:rest
      (x:) <$> loop x
    go :: V.MVector RealWorld Int -> Int -> IO Int
    go vector current = do
      a <- V.read vector current
      b <- V.read vector a
      c <- V.read vector b
      after <- V.read vector c
      let destination = 
            let decide x
                  | x < 1 = decide maxElement
                  | x `elem` [a,b,c] = decide (x-1)
                  | otherwise = x
            in decide (current-1)
      V.write vector current after
      afterDestination <- V.read vector destination
      V.write vector destination a
      V.write vector c afterDestination
      pure after
    
    maxElement = maximum ints

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ !a = a
applyNTimes x f !a = applyNTimes (pred x) f $ f a

solution1 :: IO ()
solution1 = do
  input <- fmap ((\x -> x-48) . fromEnum) . take 9 <$> readFile "inputs/day23/exampleInput"
  putStrLn $ extractSolution1 $ executeMoves 10 input

solution2 :: IO ()
solution2 = do
  input <- fmap ((\x -> x-48) . fromEnum) . take 9 <$> readFile "inputs/day23/input1"
  executeMovesP2 10000000 (input++[10..1000000]) >>= print
