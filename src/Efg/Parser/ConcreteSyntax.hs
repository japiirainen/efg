{-# LANGUAGE TupleSections #-}

module Efg.Parser.ConcreteSyntax where

import Data.Char (isDigit)
import Efg.Errors
import Efg.Parser.Lexing
import Efg.Syntax
import Text.Megaparsec (ParseError, ParseErrorBundle (ParseErrorBundle), (<?>))

import qualified Control.Monad.Combinators.Expr as Expr
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec

parseUModule :: ModuleSrcName -> Text -> UModule
parseUModule name src =
  let blocks = runParserUnsafe (show name) src srcBlocks
   in UModule name [] blocks

srcBlocks :: Parser [SrcBlock]
srcBlocks = Megaparsec.manyTill (srcBlock <* outputLines) Megaparsec.eof

srcBlockUnsafe :: Text -> SrcBlock
srcBlockUnsafe src = runParserUnsafe "<unknown>" src srcBlock

srcBlock :: Parser SrcBlock
srcBlock = do
  offset <- Megaparsec.getOffset
  pos <- Megaparsec.getSourcePos
  (src, b) <- withSource $ Megaparsec.withRecovery recover do srcBlock'
  pure $ SrcBlock (Megaparsec.unPos $ Megaparsec.sourceLine pos) offset src b

recover :: ParseError Text Void -> Parser SrcBlock'
recover e = do
  pos <- Megaparsec.statePosState <$> Megaparsec.getParserState
  reachedEOF <-
    Megaparsec.try (mayBreak sc >> Megaparsec.eof >> pure True)
      <|> pure False
  consumeTillBreak
  let err = Megaparsec.errorBundlePretty (ParseErrorBundle (e :| []) pos)
  pure $ UnParsable reachedEOF err

consumeTillBreak :: Parser ()
consumeTillBreak =
  void $
    Megaparsec.manyTill Megaparsec.anySingle $
      Megaparsec.eof
        <|> void (Megaparsec.try (eol >> Megaparsec.eof))

srcBlock' :: Parser SrcBlock'
srcBlock' = topLetOrExpr <* eolf

topLetOrExpr :: Parser SrcBlock'
topLetOrExpr =
  withSrc topLet >>= \case
    -- TODO: commands
    d -> pure (TopDecl d)

topLet :: Parser CTopDecl'
topLet = do
  CDecl <$> cDecl

cDecl :: Parser CDecl'
cDecl = simpleLet

simpleLet :: Parser CDecl'
simpleLet = do
  lhs <- cGroupNoEqual
  next <- nextChar
  case next of
    -- '=' -> sym "=" >> CLet lhs <$> cBlock
    _ -> pure (CExpr lhs)

cGroup :: Parser Group
cGroup = makeExprParser leafGroup ops

cGroupNoEqual :: Parser Group
cGroupNoEqual = makeExprParser leafGroup $ withoutOp "=" ops

leafGroup :: Parser Group
leafGroup =
  let leafGroup' = withSrc do
        next <- nextChar
        case next of
          '(' -> (CIdentifier <$> symName) <|> cParens
          '{' -> cBraces
          '[' -> cBrackets
          '\'' -> CChar <$> charLit
          _ | isDigit next -> (CNat <$> natLit) <|> (CInt <$> intLit) <|> (CFloat <$> doubleLit)
          _ -> cIdentifier
   in do
        l <- leafGroup'
        postOps <- many postFixGroup
        pure $ foldl' (\acc (op, opLhs) -> joinSrc acc opLhs (CBin (WithSrc Nothing op) acc opLhs)) l postOps

postFixGroup :: Parser (Bin', Group)
postFixGroup =
  noGap
    >> (Dot,) <$> Megaparsec.try (Megaparsec.char '.' >> withSrc cFieldName)

cFieldName :: Parser Group'
cFieldName = cIdentifier <|> (CNat <$> natLit)

cIdentifier :: Parser Group'
cIdentifier = CIdentifier <$> anyName

cParens :: Parser Group'
cParens = CParens <$> parens (commaSep cGroup)

cBrackets :: Parser Group'
cBrackets = CBrackets <$> brackets (commaSep cGroup)

cBraces :: Parser Group'
cBraces = CBraces <$> braces (commaSep cGroup)

noGap :: Parser ()
noGap =
  precededByWhitespace >>= \case
    True -> fail "Unexpected whitespace"
    False -> pass

commaSep :: Parser a -> Parser [a]
commaSep p = p `Megaparsec.sepBy` sym ","

type PrecTable a = [[(Text, Expr.Operator Parser a)]]

makeExprParser :: Parser a -> PrecTable a -> Parser a
makeExprParser p tbl = Expr.makeExprParser p tbl'
  where
    tbl' = map (map snd) tbl

withoutOp :: Text -> PrecTable a -> PrecTable a
withoutOp op = map (filter ((/= op) . fst))

ops :: PrecTable Group
ops =
  [ [("other", anySymOp)]
  , [symOpL "*", symOpL "/"]
  , [symOpL "+", symOpL "-"]
  , [symOpN ">", symOpN ">=", symOpN "<", symOpN "<="]
  , [symOpN "==", symOpN "!="]
  , [symOpL "&&"]
  , [symOpL "||"]
  ]

opWithSrc ::
  Parser (SrcPos -> a -> a -> a) ->
  Parser (a -> a -> a)
opWithSrc p = do
  (f, pos) <- withPos p
  pure (f pos)

anySymOp :: Expr.Operator Parser Group
anySymOp = Expr.InfixL $ opWithSrc do
  s <- mayBreak anySym <?> "infix operator"
  pure (binApp $ readBin s)

infixSym :: Text -> Parser ()
infixSym s = mayBreak $ sym (toText s)

symOp :: Text -> Parser (Group -> Group -> Group)
symOp s =
  opWithSrc $
    ( infixSym s
        <?> "infix operator"
    )
      >> pure (binApp $ readBin s)

symOpN :: Text -> (Text, Expr.Operator Parser Group)
symOpN s = (s, Expr.InfixN $ symOp s)

symOpL :: Text -> (Text, Expr.Operator Parser Group)
symOpL s = (s, Expr.InfixL $ symOp s)

binApp :: Bin' -> SrcPos -> Group -> Group -> Group
binApp op pos x y = joinSrc3 op' x y $ CBin op' x y
  where
    op' = WithSrc (Just pos) op

joinPos :: Maybe SrcPos -> Maybe SrcPos -> Maybe SrcPos
joinPos p Nothing = p
joinPos Nothing p = p
joinPos (Just (l, c)) (Just (l', c')) = Just (min l l', max c c')

joinSrc :: WithSrc a -> WithSrc b -> c -> WithSrc c
joinSrc (WithSrc p _) (WithSrc p' _) = WithSrc (joinPos p p')

joinSrc3 :: WithSrc a -> WithSrc b -> WithSrc c -> d -> WithSrc d
joinSrc3 (WithSrc p _) (WithSrc p' _) (WithSrc p'' _) =
  WithSrc (concatPos [p, p', p''])

concatPos :: [Maybe SrcPos] -> Maybe SrcPos
concatPos = foldl' joinPos Nothing

withSrc :: Parser a -> Parser (WithSrc a)
withSrc p = do
  (x, pos) <- withPos p
  pure $ WithSrc (Just pos) x
