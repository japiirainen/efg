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

type LExpr = Syntax.Expr Offset

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
sourceBlock' =
  optional (try nextLine)
    *> (SBTopDecl <$> topDecl)

topDecl :: Parser (TopDecl Offset)
topDecl = topDecl' <* eolf
  where
    topDecl' :: Parser (TopDecl Offset)
    topDecl' =
      def_
        <|> TopExpr <$> expr_

expr_ :: Parser LExpr
expr_ = pGroup -- TODO

def_ :: Parser (TopDecl Offset)
def_ = do
  keyword "def"
  name <- pName
  sym "="
  body <- try (withIndent expr_) <|> expr_
  return TopDef {..}

pGroup :: Parser LExpr
pGroup = makeExprParser leafGroup ops

leafGroup :: Parser LExpr
leafGroup = do
  next <- nextChar
  case next of
    '(' -> parens pGroup
    _ | Char.isDigit next -> pNum
    '\"' -> pStr
    '\\' -> pLam
    'i' -> pIf <|> pVariable
    't' -> pBool BITrue <|> pVariable
    'f' -> pBool BIFalse <|> pVariable
    _ -> pVariable

pBool :: BuiltinValue -> Parser LExpr
pBool biv = withOffset \location -> do
  builtinValue biv
  let scalar = Syntax.Bool (biv == BITrue)
  return $ Syntax.Scalar {..}

pLam :: Parser LExpr
pLam = mayNotBreak $ withOffset \location -> do
  sym "\\"
  (name, (start, _)) <- withPos pName
  mayNotBreak (sym ".")
  let nameLocation = Offset start
  body <- pGroup
  return $ Syntax.Lambda {..}

pIf :: Parser LExpr
pIf = mayNotBreak $ withOffset \location -> do
  keyword "if"
  predicate <- pGroup
  keyword "then"
  ifTrue <- pGroup
  keyword "else"
  ifFalse <- pGroup
  return $ Syntax.If {..}

pVariable :: Parser LExpr
pVariable = withOffset \location -> do
  name <- pName
  return $ Syntax.Variable {..}

pStr :: Parser LExpr
pStr = withOffset \location -> do
  s <- strLit
  let scalar = Syntax.Text (toText s)
  return $ Syntax.Scalar {..}

pNum :: Parser LExpr
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

ops :: PrecTable LExpr
ops =
  [ [juxt]
  , [symOpL "*", symOpL "/"]
  , [symOpL "+", symOpL "-"]
  , [symOpN "<", symOpN "<=", symOpN ">", symOpN ">="]
  , [symOpN "==", symOpN "!="]
  , [symOpL "&&"]
  , [symOpL "||"]
  ]

juxt :: (Text, Expr.Operator Parser LExpr)
juxt = ("space", Expr.InfixL $ withOffset \location -> sc $> funApp location)

funApp :: Offset -> LExpr -> LExpr -> LExpr
funApp location function argument = Syntax.Application {..}

symOpL :: Text -> (Text, Expr.Operator Parser LExpr)
symOpL s = (s, Expr.InfixL (symOp s))

symOpN :: Text -> (Text, Expr.Operator Parser LExpr)
symOpN s = (s, Expr.InfixN (symOp s))

infixSym :: Text -> Parser ()
infixSym s = mayBreak $ sym $ toText s

symOp :: Text -> Parser (LExpr -> LExpr -> LExpr)
symOp s = withOffset \location -> do
  (_, (start, _)) <- withPos $ label "infix operator" (infixSym s)
  return $ binApp (fromString (toString s)) (Offset start) location

binApp :: Syntax.Operator -> Offset -> Offset -> LExpr -> LExpr -> LExpr
binApp operator operatorLocation location left right = Syntax.Operator {..}
