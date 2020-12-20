module Day19.Main where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Data.Text.IO as T
import Data.Text(Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Data.IntMap as IM
import Control.Applicative
    ( Applicative(liftA2), Alternative(many, (<|>)) )
import qualified Text.ParserCombinators.ReadP as R

type Parser = P.Parsec Void Text
type CreateParser = P.Parsec Void Text

between :: Applicative m => m before -> m after -> m between -> m between
between a c b = a *> (const <$> b <*> c)

data Rule = TextRule Text | RuleId Int deriving Show

newtype RuleParser = RuleParser (IM.IntMap [[Rule]]) deriving Show

ruleParser :: Parser RuleParser
ruleParser = do
  rules <- many $ do
    ruleNumber <- P.decimal
    P.char ':'
    P.hspace
    ruleParser <- many1 $ do
      let str = TextRule <$> between (P.char '"') (P.char '"') (P.takeWhileP Nothing (/= '"'))
          rule = RuleId <$> P.decimal 
      parser <- many1 $ do
        p <- str <|> rule
        P.hspace 
        pure p
      P.char '|' <|> pure '\n'
      P.hspace
      pure parser
    () <$ P.newline <|> P.eof
    pure (ruleNumber, ruleParser)
  pure $ RuleParser $ IM.fromList rules

validInputReadP :: RuleParser -> R.ReadP ()
validInputReadP (RuleParser rules) = go 0 *> R.eof
  where
    go :: Int -> R.ReadP ()
    go ruleId = 
      let ruleToParser (TextRule t) = () <$ R.string (T.unpack t)
          ruleToParser (RuleId id') = go id'
      in do
        let parsers = fmap (fmap ruleToParser) $ rules IM.! ruleId
        rules <- R.choice $ fmap (foldl1 (liftA2 (<>)) . fmap ruleToParser) $ rules IM.! ruleId
        pure $! rules

many1 :: (MonadFail f, Monad f, Alternative f) => f a -> f [a]
many1 p = do
  p <- many p
  case p of
    [] -> fail "Not enough matched"
    p -> pure p

solution1 :: IO ()
solution1 = do
  (createInput,otherInput) <- T.breakOn "\n\n" <$>  T.readFile "inputs/day19/input1"
  rules <- either (fail . P.errorBundlePretty) pure $! P.parse ruleParser "Create Parser" createInput 
  let 
      input = drop 2 $ T.lines otherInput
      valid = not . null . R.readP_to_S (validInputReadP rules) . T.unpack <$> input
  print $ sum $ fmap fromEnum valid

solution2 :: IO ()
solution2 = do
  (createInput,otherInput) <- T.breakOn "\n\n" <$>  T.readFile "inputs/day19/input1"
  rules <- either (fail . P.errorBundlePretty) pure $! P.parse ruleParser "Create Parser" createInput 
  let 
      input = drop 2 $ T.lines otherInput
      valid = not . null . R.readP_to_S (validInputReadP $ replaceRules rules) . T.unpack <$> input
  print $ sum $ fmap fromEnum valid

replaceRules :: RuleParser -> RuleParser
replaceRules (RuleParser rules) = RuleParser $ 
  IM.insert 8 [[RuleId 42],[RuleId 42,RuleId 8 ]] $
  IM.insert 11 [[RuleId 42,RuleId 31],[RuleId 42,RuleId 11,RuleId 31]] $
  rules
