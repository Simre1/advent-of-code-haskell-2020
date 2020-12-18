module Day18.Main where

import Control.Applicative ( Alternative(many, (<|>)) )
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void ( Void )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Data.Monoid (Sum(Sum))

type Parser = P.Parsec Void Text

-- Parses a line immediately calculates the solution
-- 1 -> + and * have the same precedence
-- 2 -> + binds stronger than *
parser :: Int -> Parser Int
parser i = do
  expr <- unaryExpr >>= nextExpr i
  () <$ P.newline <|> P.eof <|> pure ()
  pure expr
  where
    unaryExpr = do
      P.hspace
      P.choice [P.decimal, brackets]
    nextExpr 1 expr1 = do
      P.hspace
      let nextOp = do
            P.hspace
            op <- ((+) <$ P.char '+') <|> ((*) <$ P.char '*')
            P.hspace
            op expr1 <$> P.choice [P.decimal, brackets] -- Consume next literal immediately
      (nextOp >>= nextExpr i) <|> pure expr1
    nextExpr 2 expr1 = do
      P.hspace
      let nextOp = do
            P.hspace
            let plus = do
                  P.char '+'
                  P.hspace
                  (+) expr1 <$> P.choice [P.decimal, brackets] -- Consume next literal immediately for addition
                mult = do
                  P.hspace
                  P.char '*'
                  P.hspace
                  (*) expr1 <$> (unaryExpr >>= nextExpr 2) -- Fully calculate the right hand side and then multiply
            mult <|> plus
      (nextOp >>= nextExpr i) <|> pure expr1
    brackets = do
      P.hspace *> P.char '('
      res <- P.hspace *> (unaryExpr >>= nextExpr i)
      P.hspace *> P.char ')'
      pure res

solution1 :: IO ()
solution1 = do
  exprs <- T.readFile "inputs/day18/input1" >>= either (fail . P.errorBundlePretty) pure . P.parse (many $ parser 1) "Parser"
  print $ sum exprs

solution2 :: IO ()
solution2 = do
  exprs <- T.readFile "inputs/day18/input1" >>= either (fail . P.errorBundlePretty) pure . P.parse (many $ parser 2) "Parser"
  print $ sum exprs