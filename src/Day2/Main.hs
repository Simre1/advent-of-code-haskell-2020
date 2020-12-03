module Day2.Main where

import qualified Data.Attoparsec.Text as P
import Data.Text (index, singleton,  Text, count)
import Data.Text.Read ( decimal )
import Data.Maybe (isJust)
import qualified Data.Text.IO as T
import Control.Applicative ( Alternative((<|>)) )
import Data.Foldable

inputParser :: P.Parser [(Int, Int, Char, Text)]
inputParser = do
  firstNumber <- P.takeWhile (/= '-')
  P.char '-'
  secondNumber <- P.takeWhile (/= ' ')
  P.char ' '
  char <- P.anyChar
  P.char ':'
  P.char ' '
  password <- P.takeWhile (/='\n')
  result <- either fail pure $ (,,,) 
                <$> fmap fst (decimal firstNumber) 
                <*> fmap fst (decimal secondNumber)
                <*> pure char 
                <*> pure password
  P.char '\n' <|> pure 'a'
  nextChar <- P.peekChar
  (result:) <$> if isJust nextChar 
    then inputParser
    else pure []

runSolution :: ((Int, Int, Char, Text) -> Bool) -> IO ()
runSolution checkPassword = do
  passwords <- either error id . P.parseOnly inputParser <$> T.readFile "inputs/day2/input1"
  let validPasswords = foldl' (\a p -> if checkPassword p then succ a else a) 0 passwords
  print validPasswords

solution1 :: IO ()
solution1 = runSolution $ \(min, max, char, t) ->
  let actual = count (singleton char) t
  in min <= actual && actual <= max

solution2 :: IO ()
solution2 = runSolution $ \(min, max, char, t) ->
  let res1 = t `index` pred min == char
      res2 = t `index` pred max == char
  in (res1 || res2) && not (res1 && res2)
