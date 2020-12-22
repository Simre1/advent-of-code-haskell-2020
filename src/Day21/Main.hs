module Day21.Main where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Control.Monad ( when )
import Control.Applicative ( Alternative((<|>)) )
import qualified Data.Text.IO as T
import qualified Data.Text as T

import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import Data.Foldable ( Foldable(foldl') )
import Data.List (sortOn)

parseInput :: Text -> [([Text], [Text])]
parseInput = 
  either error id . P.parseOnly parser
  where 
    parser :: P.Parser [([Text], [Text])]
    parser =
      P.many1 $ do
        ingredients <- P.many1 $ do
          c <- P.peekChar
          when (c == Just '(' || c == Nothing) $ fail "end of ingredients reached"
          w <- P.takeWhile (/= ' ')
          P.skipSpace
          pure w
        allergies <- do
          P.string "(contains"
          P.many1 $ do
            P.char ' '
            w <- P.takeTill (\c -> c == ',' || c == ')')
            P.string "," <|> P.string ")"
            pure w
        P.endOfLine <|> P.endOfInput 
        
        pure (ingredients, allergies)

intoHashMap :: [([Text], [Text])] -> H.HashMap Text (S.Set Text)
intoHashMap [] = H.empty
intoHashMap ((ingredients, allergies):xs) = 
  let hashMap = intoHashMap xs
      ingredientSet = S.fromList ingredients
      update = \case
        Nothing -> Just ingredientSet
        Just s -> Just $ s `S.intersection` ingredientSet
  in foldl' (\m a -> H.alter update a m) hashMap allergies
  
reduceHashMap :: H.HashMap Text (S.Set Text) -> H.HashMap Text Text
reduceHashMap h = 
  case H.foldlWithKey' (\a k v -> a <|> if S.size v == 1 then Just (k,v) else Nothing) Nothing h of
    Nothing -> H.empty
    Just (k,v) -> H.insert k (head $ S.toList v) (reduceHashMap $ H.map (`S.difference` v) $ H.delete k h)

solution1 :: IO ()
solution1 = do
  input <- parseInput <$> T.readFile "inputs/day21/input1" 
  let allergyIngredients = H.elems $ reduceHashMap $ intoHashMap input
  print $ length $ filter (not . (`elem` allergyIngredients)) $ mconcat $ fmap fst input

solution2 :: IO ()
solution2 = do
  input <- parseInput <$> T.readFile "inputs/day21/input1" 
  let allergyIngredients = H.toList $ reduceHashMap $ intoHashMap input
  print $ T.intercalate "," $ fmap snd $ sortOn fst allergyIngredients