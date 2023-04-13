{-# LANGUAGE BlockArguments #-}

module Efg.Parser.Parsing where

import Efg.Parser.Lexing
import Efg.Parser.Location (Offset (..))
import Efg.Syntax (SourceBlock (SourceBlock), SourceBlock' (..), TopDecl (..))
import Text.Megaparsec hiding (ParseError, parse)
import Text.Megaparsec.Error

import qualified Control.Monad.Combinators.Expr as Expr
import qualified Data.Char as Char
import qualified Efg.Syntax as Syntax

sourceBlocks :: Parser [SourceBlock Offset]
sourceBlocks = manyTill sourceBlock eof

sourceBlock :: Parser (SourceBlock Offset)
sourceBlock = do
  offset <- getOffset
  pos <- getSourcePos
  (src, block) <- withSource $ withRecovery recover sourceBlock'
  return $ SourceBlock (unPos (sourceLine pos)) offset src block

recover :: ParseError Text Void -> Parser (SourceBlock' Offset)
recover e = do
  pos <- statePosState <$> getParserState
  reachEof <-
    try (mayBreak sc >> eof >> return True)
      <|> return False
  consumeTillBreak
  let errMsg = errorBundlePretty (ParseErrorBundle (e :| []) pos)
  return $ Unparsable reachEof errMsg

consumeTillBreak :: Parser ()
consumeTillBreak = void $ manyTill anySingle $ eof <|> void (try (eol >> eol))

sourceBlock' :: Parser (SourceBlock' Offset)
sourceBlock' = SBTopDecl <$> topDecl

topDecl :: Parser (TopDecl Offset)
topDecl = topDecl' <* eolf
  where
    topDef = do
      (n, expr) <- def_
      return $ TopDef n expr
    topDecl' :: Parser (TopDecl Offset)
    topDecl' =
      TopExpr <$> expr_
        <|> topDef

expr_ :: Parser (Syntax.Expr Offset)
expr_ = pGroup -- TODO

def_ :: Parser (Text, Syntax.Expr Offset)
def_ = do
  keyword "def"
  n <- pName
  expr <- expr_
  return (n, expr)

pGroup :: Parser (Syntax.Expr Offset)
pGroup = makeExprParser leafGroup ops

leafGroup :: Parser (Syntax.Expr Offset)
leafGroup = do
  next <- nextChar
  case next of
    '(' -> parens pGroup
    _ | Char.isDigit next -> pNum
    '\"' -> pStr
    'i' -> pIf <|> pVariable
    _ -> pVariable

pIf :: Parser (Syntax.Expr Offset)
pIf = mayNotBreak $ withOffset \location -> do
  keyword "if"
  predicate <- pGroup
  keyword "then"
  ifTrue <- pGroup
  keyword "else"
  ifFalse <- pGroup
  return $ Syntax.If {..}

pVariable :: Parser (Syntax.Expr Offset)
pVariable = withOffset \location -> do
  name <- pName
  return $ Syntax.Variable {..}

pStr :: Parser (Syntax.Expr Offset)
pStr = withOffset \location -> do
  s <- strLit
  let scalar = Syntax.Text (toText s)
  return $ Syntax.Scalar {..}

pNum :: Parser (Syntax.Expr Offset)
pNum = withOffset \location -> do
  scalar <- number
  return $ Syntax.Scalar {..}
  where
    number =
      Syntax.Integer <$> natLit
        <|> Syntax.Real <$> doubleLit

type PrecTable a = [[(Text, Expr.Operator Parser a)]]

makeExprParser :: Parser a -> PrecTable a -> Parser a
makeExprParser p tbs = Expr.makeExprParser p (map (map snd) tbs)

ops :: PrecTable (Syntax.Expr Offset)
ops =
  [ [symOpL "*", symOpL "/"]
  , [symOpL "+", symOpL "-"]
  , [symOpN "<", symOpN "<=", symOpN ">", symOpN ">="]
  , [symOpN "==", symOpN "!="]
  ]

symOpL :: Text -> (Text, Expr.Operator Parser (Syntax.Expr Offset))
symOpL s = (s, Expr.InfixL (symOp s))

symOpN :: Text -> (Text, Expr.Operator Parser (Syntax.Expr Offset))
symOpN s = (s, Expr.InfixN (symOp s))

infixSym :: Text -> Parser ()
infixSym s = mayBreak $ sym $ toText s

symOp :: Text -> Parser (Syntax.Expr Offset -> Syntax.Expr Offset -> Syntax.Expr Offset)
symOp s = withOffset \location -> do
  label "infix operator" (infixSym s)
  return $ binApp (fromString (toString s)) location

binApp :: Syntax.Operator -> Offset -> Syntax.Expr Offset -> Syntax.Expr Offset -> Syntax.Expr Offset
binApp operator location left right = Syntax.Operator {operatorLocation = location, ..}
