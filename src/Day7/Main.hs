module Day7.Main where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text.IO as T
import Control.Applicative
    ( Applicative(liftA2), Alternative((<|>), many) )
import qualified Data.Map as M

newtype Bags = Bags (M.Map Text [(Int, Text)]) deriving Show

parseInput :: P.Parser [(Text, [(Int, Text)])]
parseInput = many $ do
  name <- liftA2 (<>) word word
  P.string " bags contain "
  let hasBags = P.many1 $ do
        amount <- many (P.char ' ') *> P.decimal
        bag <- liftA2 (<>) word word
        P.choice [P.string " bags", P.string " bag"]
        () <$ P.string ", " <|> pure ()
        pure (amount, bag)
      noBags = do
        P.string "no other bags"
        pure []
  bags <- P.choice [hasBags, noBags]
  P.char '.' *> (P.endOfLine <|> P.endOfInput)
  pure (name, bags)
  where
    word = many (P.char ' ') *> P.takeWhile (/= ' ')

childOf :: Bags -> Text -> Text -> Bool
childOf (Bags bags) child parent = 
  let subBags = bags M.! parent
      possibleParents = (snd <$> subBags)
  in or $ (child `elem` possibleParents):(childOf (Bags bags) child <$> possibleParents)

parentsOf :: Bags -> Text -> [Text]
parentsOf bags@(Bags bagMap) child = filter (childOf bags child) $ M.keys bagMap

numberOfchildBags :: Bags -> Text -> Int
numberOfchildBags (Bags bagMap) child = go child
  where 
    go child = 
      let subBags = bagMap M.! child
          childBags = sum $ fmap (\(i,n) -> i * (go n + 1)) subBags
      in childBags

solution1 :: IO ()
solution1 = do
  bagList <- T.readFile "inputs/day7/input1" >>= either fail pure . P.parseOnly parseInput 
  let bags = Bags (M.fromList bagList)
  print $ length $ parentsOf bags "shinygold"

solution2 :: IO ()
solution2 = do
  bagList <- T.readFile "inputs/day7/input1" >>= either fail pure . P.parseOnly parseInput 
  let bags = Bags (M.fromList bagList)
  print $ numberOfchildBags bags "shinygold"
