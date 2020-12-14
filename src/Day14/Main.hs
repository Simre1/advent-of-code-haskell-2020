module Day14.Main where

import Data.Word ( Word64 )
import qualified Data.Map as M
import Data.Bits
    ( Bits(setBit, complement, (.&.), (.|.), testBit) )
import Data.List ( foldl' )

data Input = Mask Word64 Word64 | Mem Word64 Word64 deriving Show

convert :: Num a => [Char] -> a
convert = convert' . reverse

convert' :: Num a => String -> a
convert' [] = 0
convert' (x : xs) = toNum x + 2 * convert' xs
  where toNum '1' = 1
        toNum _ = 0

parseInput :: String -> [Input]
parseInput str = makeInput <$> lines str
  where 
    makeInput = \case
      ('m':'a':'s':'k':' ':'=':' ':mask) -> Mask
          (convert $ flip fmap mask $ \case
          'X' -> '1'
          _ -> '0')
          (convert $ flip fmap mask $ \case
          'X' -> '0'
          x -> x)
      ('m':'e':'m':'[':str) -> 
        let addr = takeWhile (/= ']') str
            value = read $ drop (length addr + 4) str
        in Mem (read addr) value

solution1 :: IO ()
solution1 = do
  input <- parseInput <$> readFile "inputs/day14/input1"
  let (masks, memory) = foldl' process ((maxBound,0), M.empty) input
  print $ M.foldl' (+) 0 memory
  where 
    process :: ((Word64,Word64), M.Map Word64 Word64) -> Input -> ((Word64,Word64), M.Map Word64 Word64)
    process ((m1,m2), memory) = \case
      Mask m1' m2' -> ((m1',m2'), memory)
      Mem i w -> let result = (w .&. m1) .|. m2 in ((m1,m2), M.insert i result memory)


solution2 :: IO ()
solution2 = do
  input <- parseInput <$> readFile "inputs/day14/input1"
  let (masks, memory) = foldl' process ((maxBound,0), M.empty) input
  print $ M.foldl' (+) 0 memory
  where 
    process :: ((Word64,Word64), M.Map Word64 Word64) -> Input -> ((Word64,Word64), M.Map Word64 Word64)
    process ((m1,m2), memory) = \case
      Mask m1' m2' -> ((m1',m2'), memory)
      Mem k w -> let addresses = (\m -> (k .&. complement m1) .|. m .|. m2) <$> floatingAddrs m1 in 
        ((m1,m2), foldl' (\mem k -> M.insert k w mem) memory addresses)

floatingAddrs :: Word64 -> [Word64]
floatingAddrs w = 
  let values = concatMap (\i -> [setBit minBound i | testBit w i]) [0..63]
      combinations [] = [0]
      combinations (x:xs) = fmap (x+) (combinations xs) ++ combinations xs
  in combinations values