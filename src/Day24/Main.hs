module Day24.Main where

import Linear.V2 ( V2(..) )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable


data Direction = E | SE | SW | W | NW | NE deriving (Show, Enum)

parseLine :: String -> [Direction]
parseLine = \case
  [] -> []
  ('e':xs) -> E:parseLine xs
  ('w':xs) -> W:parseLine xs
  ('s':'e':xs) -> SE:parseLine xs
  ('s':'w':xs) -> SW:parseLine xs
  ('n':'w':xs) -> NW:parseLine xs
  ('n':'e':xs) -> NE:parseLine xs

canonicalPosition :: [Direction] -> V2 Int
canonicalPosition = foldl' addDirection (V2 0 0)

addDirection :: V2 Int -> Direction -> V2 Int
addDirection = flip $ (+) . \case
  E -> V2 2 0
  SE -> V2 1 (-1)
  SW -> V2 (-1) (-1)
  W -> V2 (-2) 0
  NW -> V2 (-1) 1
  NE -> V2 1 1

toTileSet :: [V2 Int] -> S.Set (V2 Int)
toTileSet = foldl' insert S.empty
  where 
    insert set b
      | S.member b set = S.delete b set
      | otherwise = S.insert b set

dailyFlip :: S.Set (V2 Int) -> S.Set (V2 Int)
dailyFlip blackTiles = 
  let whiteTiles = adjacentWhiteTiles blackTiles
      additionalBlackTiles = S.filter (\tile -> S.size (S.intersection (adjacentTiles tile) blackTiles) == 2) whiteTiles
      newWhiteTiles = S.filter (\tile -> let s = S.size (S.intersection (adjacentTiles tile) blackTiles) in s == 0 || s > 2) blackTiles
  in S.union (S.difference blackTiles newWhiteTiles) additionalBlackTiles
  where
    adjacentWhiteTiles :: S.Set (V2 Int) -> S.Set (V2 Int)
    adjacentWhiteTiles set = S.difference (foldMap adjacentTiles set) blackTiles
    adjacentTiles :: V2 Int -> S.Set (V2 Int)
    adjacentTiles tile = S.fromList $ addDirection tile <$> [E .. NE]

solution1 :: IO ()
solution1 = do
  input <- fmap parseLine . lines <$> readFile "inputs/day24/input1"
  let tiles = toTileSet $ canonicalPosition <$> input
  print $ S.size tiles

solution2 :: IO ()
solution2 = do
  input <- fmap parseLine . lines <$> readFile "inputs/day24/input1"
  let tiles = toTileSet $ canonicalPosition <$> input
  print $ S.size $ iterate dailyFlip tiles !! 100