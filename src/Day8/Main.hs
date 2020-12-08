{-# LANGUAGE ViewPatterns #-}
module Day8.Main where

import qualified Data.IntMap as IM
import Debug.Trace (traceShowId)
import Data.List (foldl')
import Control.Applicative
import Data.Maybe (fromMaybe, fromJust)

data Operation
  = NoOperation Int
  | Accumulate Int
  | Jump Int

newtype Memory = Memory (IM.IntMap Operation)

-- (Line Number, Operation, Accumulator) -> m
newtype Debug m = Debug ((Int, Operation, Int) -> m)

boot :: Monoid m => Debug m -> Memory -> (m, Either String Int)
boot (Debug f) (Memory operations) = execute 0 0
  where
    execute acc n = 
      let currentOperation = IM.lookup n operations
          debugInfo = maybe mempty (\x -> f (n, x, acc)) currentOperation 
          (debug, solution) = case currentOperation of
            Nothing -> if IM.member (pred n) operations
              then (mempty, Right acc)
              else (mempty, Left "ERR: Index out of bounds")
            Just (NoOperation _) -> execute acc (succ n)
            Just (Accumulate i) -> execute (acc + i) (succ n)
            Just (Jump i) -> execute acc (n + i)
      in (debugInfo <> debug, solution)

parseMemory :: String -> Memory
parseMemory = Memory . IM.fromList . zip [0..] . fmap toOperation . lines 
  where 
    toOperation str = case take 3 str of
      "nop" -> NoOperation (sign (str !! 4) $ read $ drop 5 str)
      "acc" -> Accumulate (sign (str !! 4) $ read $ drop 5 str)
      "jmp" -> Jump (sign (str !! 4) $ read $ drop 5 str)
    sign '+' = id
    sign '-' = negate

firstDuplicate :: [(Int,a)] -> Maybe (Int,a)
firstDuplicate xs = go xs IM.empty
  where go ((i,a):xs) s = 
          if IM.member i s
            then Just (i,a)
            else go xs (IM.insert i a s)
        go [] _ = Nothing

changeMemory :: Memory -> [Memory]
changeMemory (Memory map) = (\x -> Memory $ IM.insert x (replace x) map) <$> IM.keys map 
  where 
    replace n = case map IM.! n of
      NoOperation x -> Jump x
      Jump x -> NoOperation x
      x -> x

solution1 :: IO ()
solution1 = do
  memory <- parseMemory <$> readFile "inputs/day8/input1"
  let (debug, _) = boot (Debug $ \(i,_,a) -> [(i,a)]) memory
  print $ snd <$> firstDuplicate debug

solution2 :: IO ()
solution2 = do
  memory <- parseMemory <$> readFile "inputs/day8/input1"
  let maybeSolutions = flip fmap (changeMemory memory) $ \mem -> 
        let (debug, a) = boot (Debug $ \(i,_,a) -> [(i,a)]) mem
            dup = firstDuplicate debug
        in case dup of
            Nothing -> Just a
            Just _ -> Nothing
  print $ foldl' (<|>) empty maybeSolutions  

