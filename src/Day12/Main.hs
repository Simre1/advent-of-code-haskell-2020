module Day12.Main where


import Data.Foldable ( Foldable(foldl') )
data Instruction = InstrDirection Direction Int | InstrTurn Turn Int | InstrForward Int

data Direction = North | East | South | West deriving (Enum, Show)
data Turn = TurnRight | TurnLeft

turnRight :: Direction -> Direction
turnRight dir = toEnum $ (fromEnum dir + 1) `mod` 4

turnLeft :: Direction -> Direction
turnLeft dir = toEnum $ (fromEnum dir - 1) `mod` 4

data Ship = Ship Int Int Direction deriving Show

data ShipWaypoint = ShipWaypoint Int Int Waypoint deriving Show

data Waypoint = Waypoint Int Int deriving Show

rotateRight :: Waypoint -> Waypoint
rotateRight (Waypoint x y) = Waypoint y (-x)

rotateLeft :: Waypoint -> Waypoint
rotateLeft (Waypoint x y) = Waypoint (-y) x

parseInput :: String -> [Instruction]
parseInput str = flip fmap (lines str) $ \case
  ('F':xs) -> InstrForward (read xs)
  ('R':xs) -> InstrTurn TurnRight (read xs `quot` 90)
  ('L':xs) -> InstrTurn TurnLeft (read xs `quot` 90)
  ('N':xs) -> InstrDirection North (read xs)
  ('S':xs) -> InstrDirection South (read xs)
  ('E':xs) -> InstrDirection East (read xs)
  ('W':xs) -> InstrDirection West (read xs)

executeInstruction :: Instruction -> Ship -> Ship
executeInstruction = flip $ \(Ship x y dir) -> \case
  InstrDirection d i -> case d of
     North -> Ship x (y+i) dir
     East -> Ship (x+i) y dir
     South -> Ship x (y-i) dir
     West -> Ship (x-i) y dir
  InstrTurn t i -> 
    let fs = replicate i $ case t of
              TurnRight -> turnRight
              TurnLeft -> turnLeft
    in Ship x y . foldl' (.) id fs $ dir
  InstrForward i -> executeInstruction (InstrDirection dir i) (Ship x y dir)

executeWaypointInstruction :: Instruction -> ShipWaypoint -> ShipWaypoint
executeWaypointInstruction = flip $ \(ShipWaypoint x y (Waypoint wX wY)) -> \case
  InstrDirection d i -> case d of
     North -> ShipWaypoint x y $ Waypoint wX (wY+i)
     East -> ShipWaypoint x y $ Waypoint (wX+i) wY
     South -> ShipWaypoint x y $ Waypoint wX (wY-i)
     West -> ShipWaypoint x y $ Waypoint (wX-i) wY
  InstrTurn t i -> 
    let fs = replicate i $ case t of
              TurnRight -> rotateRight
              TurnLeft -> rotateLeft
    in ShipWaypoint x y . foldl' (.) id fs $ Waypoint wX wY
  InstrForward 0 -> ShipWaypoint x y (Waypoint wX wY)
  InstrForward i -> executeWaypointInstruction (InstrForward $ pred i) $ ShipWaypoint (x+wX) (y+wY) (Waypoint wX wY)


solution1 :: IO ()
solution1 = do
  instructions <- parseInput <$> readFile "inputs/day12/input1"
  let (Ship x y _) = foldl' (flip executeInstruction) (Ship 0 0 East) instructions
  print (abs x + abs y)


solution2 :: IO ()
solution2 = do
  instructions <- parseInput <$> readFile "inputs/day12/input1"
  let (ShipWaypoint x y _) = foldl' (flip executeWaypointInstruction) (ShipWaypoint 0 0 (Waypoint 10 1)) instructions
  print (abs x + abs y)
