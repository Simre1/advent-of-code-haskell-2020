module Day20.Main where

import qualified Data.Attoparsec.Text as P
import qualified Data.IntMap as IM
import qualified Data.Massiv.Array as A
import Data.Word ( Word16 )
import Data.Text (Text)
import Control.Applicative ( Alternative((<|>), many) )
import qualified Data.Text as T
import Data.Bits ( Bits(testBit, setBit, clearBit) )
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Foldable ( Foldable(foldl'), forM_ )
import Data.List ( foldl', sortBy )
import Control.Monad ( forM_, when )

-- Incredibly inefficient. Could be sped up dramatically by not rotating the whole array when we check just for the borders.

tileSize :: Int
tileSize = 10;

parseTiles :: Text -> IM.IntMap Tile
parseTiles = IM.fromList . fmap toTile . either (error "Parsing failed") id . P.parseOnly parser
  where 
    parser :: P.Parser [(Int, Text)]
    parser = many $ do
      P.string "Tile "
      id' <- P.decimal
      P.char ':'
      t <- P.takeTill ('T'==)
      pure (id', T.replace "\n" "" t)
    toTile :: (Int, Text) -> (Int, Tile)
    toTile (id', text) = (id', makeTileFromList id' $ mapToInt <$> T.unpack text)
    mapToInt :: Char -> Int
    mapToInt '#' = 1
    mapToInt '.' = 0
    mapToInt _ = error "Malformed input!!!"

toWord16 :: [Int] -> Word16
toWord16 xs = snd $ go xs
  where 
    go :: [Int] -> (Int, Word16)
    go [] = (0,0)
    go (x:xs) = 
      let (i, w) = go xs
      in (succ i, if x /= 0 then setBit w i else clearBit w i)

tileSizeMinus1 :: Int
tileSizeMinus1 = tileSize - 1

fromWord16 :: Word16 -> [Int]
fromWord16 w = fromEnum . testBit w <$> [9,8..0]

firstFilledTile :: IM.IntMap Tile -> M.Map (Int, Int) Tile
firstFilledTile = M.singleton (0,0) . snd . head . IM.toList

makeTileFromList :: Int -> [Int] -> Tile
makeTileFromList id' = makeTile id' . A.fromList A.Seq

makeTile :: (A.Index r) => Int -> A.Array A.U r Int -> Tile
makeTile id' arr = 
    let array = A.resize' (A.Sz2 10 10) $ arr
        top = toWord16 $ fmap (array A.!) $ zipWith A.Ix2 (repeat 0) [0..tileSizeMinus1]
        bottom = toWord16 $ fmap (array A.!) $ zipWith A.Ix2 (repeat tileSizeMinus1) [0..tileSizeMinus1]
        left = toWord16 $ fmap (array A.!) $ zipWith A.Ix2 [0..tileSizeMinus1] (repeat 0)
        right = toWord16 $ fmap (array A.!) $ zipWith A.Ix2 [0..tileSizeMinus1] (repeat tileSizeMinus1)
    in (Tile id' array top bottom left right)

fillIn :: IM.IntMap Tile -> M.Map (Int, Int) Tile
fillIn tileMap =
  let (id',tile) = head $ IM.toList tileMap
  in fillMap (IM.delete id' tileMap) (M.singleton (0,0) tile)
  where 
    fillMap :: IM.IntMap Tile -> M.Map (Int, Int) Tile -> M.Map (Int, Int) Tile
    fillMap tileMap tilePositions
      | IM.null tileMap = tilePositions
      | otherwise = 
          let updatedPositions = 
                IM.foldl' (\positions currentTile -> 
                  case M.foldlWithKey' (\pos originPos originTile -> pos <|> findNeighbor positions originPos originTile currentTile) (Nothing) positions of
                   Nothing -> positions
                   Just (foundPos, tile) -> case M.lookup foundPos positions of
                     Nothing -> M.insert (foundPos) (tile) positions 
                     Just x -> error $ show (x,tile, currentTile, foundPos)
                  ) tilePositions tileMap
              removeIds = tileId <$> M.elems updatedPositions
              updatedTileMap = foldl' (\m r -> IM.delete r m) tileMap removeIds
          in 
            fillMap updatedTileMap updatedPositions

    ops = (.) <$> [id, rotateRight, rotateLeft, rotateRight . rotateRight] <*> [id, flipX, flipY]
    findNeighbor :: M.Map (Int, Int) Tile -> (Int, Int) -> Tile -> Tile -> Maybe ((Int, Int), Tile)
    findNeighbor map (x,y) origin neighbor = 
      let tilePermutations = ($neighbor) <$> ops
          f (x,y) = --if M.member (x,y-1) map then Nothing else 
            Just (\t -> ((x,y), t))
      in (f (x,y-1) <*> checkTile origin tileTop tilePermutations tileBottom) <|> 
          (f (x,y+1) <*> checkTile origin tileBottom tilePermutations tileTop) <|>
          (f (x-1,y) <*> checkTile origin tileLeft tilePermutations tileRight) <|>
          (f (x+1,y) <*> checkTile origin tileRight tilePermutations tileLeft) 
    checkTile :: Tile -> (Tile -> Word16) -> [Tile] -> (Tile -> Word16) -> Maybe Tile
    checkTile t1 f1 t2s f2 = foldl' (<|>) Nothing $ (\t2 -> if f1 t1 == f2 t2 then Just t2 else Nothing) <$> t2s


fillArray :: M.Map (Int, Int) Tile -> A.Array A.B A.Ix2 Tile
fillArray map = let s = floor $ sqrt (fromIntegral $ M.size map) in A.resize' (A.Sz2 s s) . A.fromList A.Seq . fmap snd . reorder . M.toList $ map
  where reorder = sortBy (\((x1,y1),_) ((x2,y2),_) -> 
          case y1 `compare` y2 of
            EQ -> x1 `compare` x2
            x -> x
          )
 
mergeTiles :: A.Array A.B A.Ix2 Tile -> A.Array A.U A.Ix2 Int
mergeTiles arr = 
  let dims = A.liftSz (*8) $ A.size arr
  in A.makeArray A.Seq dims $ \ix ->
    let outer = A.liftIndex (`quot` 8) ix
        inner = A.liftIndex (succ . (`mod` 8)) ix
        (Tile _ valueArr _ _ _ _) = arr A.! outer
    in valueArr A.! inner

leastRoughWater ::  A.Array A.U A.Ix2 Int -> IO Int
leastRoughWater arr = minimum <$> traverse findRoughWater (($arr) <$> ops)
  where ops = (.) <$> [id, rotateArrayRight, rotateArrayLeft, rotateArrayRight . rotateArrayRight] <*> [id, flipArrayX, flipArrayY]

findRoughWater :: A.Array A.U A.Ix2 Int -> IO Int
findRoughWater arr = do
  mArray <- A.thaw arr
  forM_ iterFields $ \pos ->
    set0OnMonster pos mArray
  arr' <- A.freeze A.Seq mArray
  pure $ A.sum arr'
  where 
    iterFields :: [A.Ix2]
    iterFields = 
      let (A.Sz2 x y) =  A.size arr
      in A.Ix2 <$> [0..x-4] <*> [0..y-21]
    isSeaMonster :: A.Ix2 -> Bool
    isSeaMonster ix = 
      let x = sum $ (arr A.!) . (+ix) . uncurry A.Ix2 <$> seaMonster
      in x == 15
    set0OnMonster :: A.Ix2 -> A.MArray (A.PrimState IO) A.U A.Ix2 Int -> IO ()
    set0OnMonster ix arr' = 
      when (isSeaMonster ix) $
        let iters = ((+ix) . uncurry A.Ix2 <$> seaMonster)
        in mapM_ (\i -> A.write arr' i 0) iters 
    seaMonster = [(1,0), (2,1), (2,4), (1,5), (1,6), (2, 7), (2,10), (1,11), (1,12), (2,13), (2,16), (1, 17), (1,18), (1,19), (0,18)]

-- Sea monster
-- ..................#.
-- #....##....##....###
-- .#..#..#..#..#..#...

rotateArrayRight :: A.Array A.U A.Ix2 Int -> A.Array A.U A.Ix2 Int
rotateArrayRight = A.compute . A.transform' (\s -> (s, s)) (\(A.Sz2 sX sY) get (A.Ix2 x y) -> get $ A.Ix2 (sY - 1 - y) x) 

rotateArrayLeft :: A.Array A.U A.Ix2 Int -> A.Array A.U A.Ix2 Int
rotateArrayLeft = rotateArrayRight . rotateArrayRight . rotateArrayRight

flipArrayY :: A.Array A.U A.Ix2 Int -> A.Array A.U A.Ix2 Int
flipArrayY = rotateArrayRight . flipArrayX . rotateArrayLeft

flipArrayX :: A.Array A.U A.Ix2 Int -> A.Array A.U A.Ix2 Int
flipArrayX = A.compute . A.transform' (\s -> (s, s)) (\(A.Sz2 sX sY) get (A.Ix2 x y) -> get $ A.Ix2 (sX - x - 1) y) 

rotateRight :: Tile -> Tile
rotateRight (Tile id' arr _ _ _ _) = makeTile id' (rotateArrayRight arr)

rotateLeft :: Tile -> Tile
rotateLeft = rotateRight . rotateRight . rotateRight

flipX :: Tile -> Tile
flipX (Tile id' arr _ _ _ _) = makeTile id' $ flipArrayX arr

flipY :: Tile -> Tile
flipY = rotateLeft . flipX . rotateRight

data Tile = Tile {
  tileId :: Int,
  tileData :: A.Array A.U A.Ix2 Int,
  tileTop :: Word16,
  tileBottom :: Word16,
  tileLeft :: Word16,
  tileRight :: Word16
  } deriving (Show, Eq)

flipNumber :: Word16 -> Word16
flipNumber = toWord16 . reverse . fromWord16

getCorners :: A.Manifest r A.Ix2 x => A.Array r A.Ix2 x -> [x]
getCorners arr = (arr A.!) <$> [(0 A.:. 0), ((x-1) A.:. 0), ((x-1) A.:. (y-1)), (0 A.:. (y-1))]
  where (A.Sz2 x y) = A.size arr

solution1 :: IO ()
solution1 = do
  inputMap <- parseTiles <$> T.readFile "inputs/day20/input1"
  let arrayIds = tileId <$> (fillArray $ fillIn inputMap)
  print $ product $ getCorners arrayIds
  pure ()

solution2 :: IO ()
solution2 = do
  inputMap <- parseTiles <$> T.readFile "inputs/day20/input1"
  let tileArray = fillArray $ fillIn inputMap
      image = rotateArrayRight $ rotateArrayRight $ rotateArrayRight $ flipArrayX $ rotateArrayRight $ mergeTiles tileArray
  leastRoughWater image >>= print
  pure ()
