module Day5.Main where
  
import Data.Bits ( Bits(clearBit, setBit) )
import Data.Foldable (Foldable(foldl'))
import Data.Semigroup (Max(Max))
import Data.List (sort)

getSeat :: String -> Int
getSeat str
  | length str == 10 =
      let row = foldl' setRow 0 $ zip [6,5..0] (take 7 str)
          column = foldl' setColumn 0 $ zip [2,1,0] (drop 7 str)
      in row * 8 + column
  | otherwise = -1 
  where 
    setRow n (pos,c) = (if c == 'B' then setBit else clearBit) n pos
    setColumn n (pos,c) = (if c == 'R' then setBit else clearBit) n pos

solution1 :: IO ()
solution1 = readFile "inputs/day5/input1" >>= print . foldMap (Max . getSeat). lines

solution2 :: IO ()
solution2 = do
  (x:xs) <- sort . fmap getSeat . lines <$> readFile "inputs/day5/input1"
  print $ findEmptySeat xs x
    where 
      findEmptySeat [] _ = -1
      findEmptySeat (x:xs) a = if x - a > 1 then a + 1 else findEmptySeat xs x