module Day18.Main where

import Control.Applicative ( Alternative(many, (<|>)) )
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void ( Void )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Data.Monoid (Sum(Sum))

data Expr
  = Lit Int
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Show, Eq)

type Parser = P.Parsec Void Text

parser :: Int -> Parser Expr
parser i = do
  expr <- unaryExpr >>= nextExpr i
  () <$ P.newline <|> P.eof <|> pure ()
  pure expr
  where
    unaryExpr = do
      P.hspace
      expr1 <- P.choice [Lit <$> P.decimal, brackets]
      P.hspace
      pure expr1
    nextExpr 1 expr1 = do
      P.hspace
      let nextOp = do
            P.hspace
            op <- (Add <$ P.char '+') <|> (Mult <$ P.char '*')
            P.hspace
            op expr1 <$> P.choice [Lit <$> P.decimal, brackets]
      (nextOp >>= nextExpr i) <|> pure expr1
    nextExpr 2 expr1 = do
      P.hspace
      let nextOp = do
            P.hspace
            let plus = do
                  P.char '+'
                  P.hspace
                  Add expr1 <$> P.choice [Lit <$> P.decimal, brackets]
                mult = do
                  P.hspace
                  P.char '*'
                  P.hspace
                  Mult expr1 <$> (unaryExpr >>= nextExpr 2)
            mult <|> plus
      (nextOp >>= nextExpr i) <|> pure expr1
    brackets = do
      P.hspace *> P.char '('
      res <- P.hspace *> (unaryExpr >>= nextExpr i)
      P.hspace *> P.char ')'
      pure res

solveExpr :: Expr -> Int
solveExpr = \case
  Lit i -> i
  Add x y -> solveExpr x + solveExpr y
  Mult x y -> solveExpr x * solveExpr y

solution1 :: IO ()
solution1 = do
  exprs <- T.readFile "inputs/day18/input1" >>= either (fail . P.errorBundlePretty) pure . P.parse (many $ parser 1) "Parser"
  print $ foldMap (Sum . solveExpr) exprs
  pure ()

solution2 :: IO ()
solution2 = do
  exprs <- T.readFile "inputs/day18/input1" >>= either (fail . P.errorBundlePretty) pure . P.parse (many $ parser 2) "Parser"
  print $ foldMap (Sum . solveExpr) exprs
  pure ()