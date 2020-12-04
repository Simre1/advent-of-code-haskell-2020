module Day4.Main where

import qualified Data.Text.Read as T
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as P
import Control.Applicative ( Alternative((<|>), many) )
import Data.Function ( (&) )
import Control.Monad ( unless )
import Data.Attoparsec.ByteString.Char8 (isDigit)
import Data.Functor (($>))
import Data.List (nubBy)

scanner :: (T.Text -> P.Parser ()) -> P.Parser Int
scanner fieldParser = do
  matches <- many $ do
    field <- P.choice $ P.string <$> ["hcl", "byr", "iyr", "eyr", "hgt", "ecl", "pid", "cid"]
    P.char ':'
    value <- (1 <$ fieldParser field) <|> (0 <$ P.takeWhile (\c -> c /= ' ' && c /= '\n'))
    (() <$ P.char ' ') <|> (() <$ P.char '\n') <|> P.endOfInput
    pure (field, value)
  let fieldCount = sum $ snd <$> nubBy (\a b -> fst a == fst b) matches
  (P.char '\n' *> scanner fieldParser) <|> pure 0 &
    if fieldCount == 7
      then fmap succ
      else id
  
solution1 :: IO ()
solution1 = do
  file <- T.readFile "inputs/day4/input1"
  print $ flip P.parseOnly file $ scanner $ \case
    "cid" -> fail "ignore cid"
    _ -> P.takeWhile (\c -> c /= ' ' && c /= '\n') $> ()

solution2 :: IO ()
solution2 = do
  file <- T.readFile "inputs/day4/input1"
  print $ flip P.parseOnly file $ scanner $ \case
      "byr" -> do
        value <- P.take 4
        num <- fst <$> either fail pure (T.decimal value)
        unless (1920 <= num && num <= 2002) (fail "byr was not in range:")
      "iyr" -> do
        value <- P.take 4
        num <- fst <$> either fail pure (T.decimal value)
        unless (2010 <= num && num <= 2020) (fail "iyr was not in range")
      "eyr" -> do
        value <- P.take 4
        num <- fst <$> either fail pure (T.decimal value)
        unless (2020 <= num && num <= 2030) (fail "eyr was not in range")
      "hgt" -> do
        num <- P.takeWhile isDigit >>= (either fail (pure . fst) . T.decimal)
        (<|>)
          (P.string "cm" *> unless (150 <= num && num <= 193) (fail "hgt not in range"))
          (P.string "in" *> unless (59 <= num && num <= 76) (fail "hgt not in range"))
        pure ()
      "hcl" -> do
        P.char '#'
        matches <- many $ P.choice $ P.char <$> ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
        unless (length matches == 6) (fail "invalid amount of characters in hcl")
      "ecl" -> do
        P.choice $ P.string <$> ["amb", "blu", "brn", "gry" ,"grn" ,"hzl","oth"]
        pure ()
      "pid" -> do
        matches <- many $ P.choice $ P.char <$> ['0','1','2','3','4','5','6','7','8','9']
        unless (length matches == 9) (fail "invalid amount of numbers in pid")
      "cid" -> fail "ignore cid"
