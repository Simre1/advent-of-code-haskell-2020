{-# LANGUAGE ViewPatterns #-}

module Day8.Main where

import Control.Applicative (Alternative (empty, (<|>)))
import qualified Data.IntMap as IM
import Data.List (foldl')

data Operation
  = NoOperation Int
  | Accumulate Int
  | Jump Int

newtype Program = Program (IM.IntMap Operation)

-- (Line Number, Operation, Accumulator) -> m
newtype Debug m = Debug ((Int, Operation, Int) -> m)

boot :: Monoid m => Debug m -> Program -> (m, Either String Int)
boot (Debug f) (Program operations) = execute 0 0
  where
    execute acc n =
      let currentOperation = IM.lookup n operations
          debugInfo = maybe mempty (\x -> f (n, x, acc)) currentOperation
          (debug, solution) = case currentOperation of
            Nothing ->
              if IM.member (pred n) operations
                then (mempty, Right acc)
                else (mempty, Left "ERR: Index out of bounds")
            Just (NoOperation _) -> execute acc (succ n)
            Just (Accumulate i) -> execute (acc + i) (succ n)
            Just (Jump i) -> execute acc (n + i)
       in (debugInfo <> debug, solution)

parseProgram :: String -> Program
parseProgram = Program . IM.fromList . zip [0 ..] . fmap toOperation . lines
  where
    toOperation str = case take 3 str of
      "nop" -> NoOperation argument
      "acc" -> Accumulate argument
      "jmp" -> Jump argument
      where
        sign '+' = id
        sign '-' = negate
        argument = sign (str !! 4) $ read $ drop 5 str

firstDuplicate :: [(Int, a)] -> Maybe (Int, a)
firstDuplicate xs = go xs IM.empty
  where
    go ((i, a) : xs) s =
      if IM.member i s
        then Just (i, a)
        else go xs (IM.insert i a s)
    go [] _ = Nothing

fixProgram :: Program -> [Program]
fixProgram (Program map) = (\x -> Program $ IM.insert x (replace x) map) <$> IM.keys map
  where
    replace n = case map IM.! n of
      NoOperation x -> Jump x
      Jump x -> NoOperation x
      x -> x

solution1 :: IO ()
solution1 = do
  program <- parseProgram <$> readFile "inputs/day8/input1"
  let (debug, _) = boot (Debug $ \(i, _, a) -> [(i, a)]) program
  print $ snd <$> firstDuplicate debug

solution2 :: IO ()
solution2 = do
  program <- parseProgram <$> readFile "inputs/day8/input1"
  let maybeSolutions = flip fmap (fixProgram program) $ \prog ->
        let (debug, a) = boot (Debug $ \(i, _, a) -> [(i, a)]) prog
            dup = firstDuplicate debug
         in case dup of
              Nothing -> Just a
              Just _ -> Nothing
  print $ foldl' (<|>) empty maybeSolutions
