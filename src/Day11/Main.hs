module Day11.Main where

import qualified Data.Vector as V
import Data.Foldable ( Foldable(foldl') )
import Data.Bool ( bool )

data Seat = Available | Occupied | NoSeat deriving (Eq, Show)

data SeatGrid = SeatGrid (V.Vector Seat) Int deriving (Eq, Show)

parseInput :: String -> SeatGrid
parseInput str = 
  let row = length . head $ lines str
      vector = flip V.unfoldr (filter (\s -> s == 'L' || s == '#' || s == '.') str) $ \case
        (x:xs) -> Just (charToSeat x,xs)
        [] -> Nothing 
  in SeatGrid vector row
  where charToSeat 'L' = Available
        charToSeat '#' = Occupied
        charToSeat '.' = NoSeat

generateOutput :: SeatGrid -> String
generateOutput (SeatGrid seats row) = V.ifoldr' (\i seat str -> (if i `mod` row == 0 then ('\n':) else id) (seatToChar seat:str)) "" seats
  where seatToChar Available = 'L'
        seatToChar Occupied = '#'
        seatToChar NoSeat = '.'

seatAt :: (Int, Int) -> SeatGrid -> Seat
seatAt (r,c) (SeatGrid seats row) = if
  | r >= row -> NoSeat
  | r < 0 -> NoSeat
  | c < 0 -> NoSeat
  | c >= V.length seats `quot` row -> NoSeat
  | otherwise -> seats V.! (row * c + r)

mapSeatGrid :: ((Int, Int) -> Seat -> Seat) -> SeatGrid -> SeatGrid
mapSeatGrid f (SeatGrid seats row) = SeatGrid (V.imap (\i s -> f (i `rem` row, i `quot` row) s) seats) row

getSeatList :: SeatGrid -> [Seat]
getSeatList (SeatGrid v _) = V.toList v

applyAdjencyRule :: SeatGrid -> SeatGrid
applyAdjencyRule seats = flip mapSeatGrid seats $ \pos s -> case s of
  NoSeat -> NoSeat
  Available -> if Occupied `notElem` adjacentSeats pos
    then Occupied
    else Available
  Occupied -> if 4 <= foldl' (flip $ \s -> if s == Occupied then succ else id) 0 (adjacentSeats pos)
    then Available
    else Occupied
  where 
    adjacentSeats (r,c) = let d = [-1,0,1] in [seatAt (r+x,c+y) seats | x <- d, y <- d, x /= 0 || y /= 0]

applyVisibiltyRule :: SeatGrid -> SeatGrid
applyVisibiltyRule seats@(SeatGrid seatVector row) = flip mapSeatGrid seats $ \pos s -> case s of
  NoSeat -> NoSeat
  Available -> if visiblePersons pos == 0
    then Occupied
    else Available
  Occupied -> if 5 <= visiblePersons pos
    then Available
    else Occupied
  where
    (maxR, maxC) = (row, V.length seatVector `quot` row)
    visiblePersons (r,c) = sum $ let d = [-1,0,1] in [findSeat (r,c) (x,y) | x <- d, y <- d, x /= 0 || y /= 0]
    findSeat (x,y) (dx,dy)
      | x >= maxR || y >= maxC || x < 0 || y < 0 = 0 
      | seatAt (x+dx,y+dy) seats == Available = 0
      | seatAt (x+dx,y+dy) seats == Occupied = 1
      | otherwise = findSeat (x+dx,y+dy) (dx,dy)

solution1 :: IO ()
solution1 = do
  seats <- parseInput <$> readFile "inputs/day11/input1"
  let loop oldSeats = 
        let newSeats = applyAdjencyRule oldSeats
        in bool (loop newSeats) newSeats (newSeats == oldSeats)
  print $ length $ filter (==Occupied) $ getSeatList $ loop seats

solution2 :: IO ()
solution2 = do
  seats <- parseInput <$> readFile "inputs/day11/input1"
  let loop oldSeats = 
        let newSeats = applyVisibiltyRule oldSeats
        in bool (loop newSeats) newSeats (newSeats == oldSeats)
  print $ length $ filter (==Occupied) $ getSeatList $ loop seats