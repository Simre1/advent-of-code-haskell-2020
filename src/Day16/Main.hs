module Day16.Main where

import Data.Bits
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as P
import Data.Word (Word32)
import Control.Applicative
import Debug.Trace (traceShowId, traceShow)
import Control.DeepSeq (force)
import Data.Foldable
import Text.Printf
import qualified Data.IntMap as IM
import Data.List (sort)
import Data.List (isInfixOf)
import qualified Data.Text as T

parser :: P.Parser ([(Text , Word32 -> Bool)], [Word32] , [[Word32]])
parser = do
  fields <- many $ do
    name <- P.takeWhile (/=':')
    P.char ':'
    let numbers = do
          many (P.char ' ')
          n1 <- P.decimal
          P.char '-'
          many (P.char ' ')
          n2 <- P.decimal
          many (P.char ' ')
          pure (n1,n2)
    (n1a,n1b) <- numbers
    P.string "or"
    (n2a,n2b) <- numbers
    P.endOfLine
    pure (name, \n -> (n1a <= n && n <= n1b) || (n2a <= n && n <= n2b) )
  P.endOfLine
  P.string "your ticket:" *> P.endOfLine
  let ticket = do
        t <- P.many1 $ do
          n <- P.decimal
          P.char ',' <|> pure ','
          pure n
        P.endOfLine <|> P.endOfInput
        pure t
  myTicket <- ticket
  P.endOfLine
  P.string "nearby tickets:" *> P.endOfLine
  otherTickets <- many ticket
  pure (fields, myTicket, otherTickets)

solution1 :: IO ()
solution1 = do
  (rules, myTicket, otherTickets) <- T.readFile "inputs/day16/input1" >>= either fail pure . P.parseOnly parser
  let results = fmap (fmap $ \a -> (a, applyRules (snd <$> rules) a)) (myTicket:otherTickets)
  print $ sum $ getInvalid results

applyRules :: [Word32 -> Bool] -> Word32 -> Word32 
applyRules fs w = snd $ foldr (\f (i,r) -> if f w then (succ i, setBit r i) else (succ i, clearBit r i)) (0,0) fs 

getInvalid :: [[(Word32, Word32)]] -> [Word32]
getInvalid = foldMap . foldMap $ \(field, res) -> [field | res == 0]

mergeResults :: [[Word32]] -> [Word32]
mergeResults = foldl' (\w1 w2 -> zipWith (.&.) w1 w2) (repeat maxBound)

trimInvalid :: [[Word32]] -> [[Word32]]
trimInvalid = foldMap $ \row -> if any (== 0) row then [] else [row]

reduceFields :: IM.IntMap Word32 -> IM.IntMap Word32 
reduceFields fields = 
  let reducePoint = foldl' (\a (k,field) -> a <|> if isSolution field then Just k else Nothing) Nothing (IM.toList fields)
  in case reducePoint of
    Nothing -> fields
    Just key ->
      let field = fields IM.! key
      in IM.insert key (field) $ reduceFields $ IM.mapWithKey (\k f -> if k == key then f else f `xor` field) (IM.delete key fields)

isSolution :: Word32 -> Bool
isSolution w = w /= maxBound && isInt (logBase 2 (fromIntegral w)) 20
  where 
    isInt x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0

solution2 :: IO ()
solution2 = do
  (rules, myTicket, otherTickets) <- T.readFile "inputs/day16/input1" >>= either fail pure . P.parseOnly parser
  let results = fmap (fmap $ applyRules (snd <$> rules)) (myTicket:otherTickets)
  let validRanges = mergeResults $ trimInvalid results
      solutions = reduceFields $ IM.fromList (zip [0..] validRanges)
      labels = fmap (\k -> let n = (firstBit $ solutions IM.! k) in (fst <$> rules) !! (length myTicket - 1 - n)) ((IM.keys solutions))
      labeledTicket = zip labels myTicket
  -- print labeledTicket
  print $ product $ fmap (fromIntegral.snd) $ filter (T.isInfixOf (T.pack "departure") . fst) $ labeledTicket

firstBit :: Word32 -> Int
firstBit w = go 0
  where go i = if testBit w i then i else go (succ i)



