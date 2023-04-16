{-# LANGUAGE BlockArguments #-}

module Efg.Parser.Parsing where

import Efg.Parser.Lexing
import Efg.Parser.Location (Offset (..))
import Text.Megaparsec hiding (ParseError, parse)
import Text.Megaparsec.Error (ParseError)

import qualified Control.Applicative.Combinators.NonEmpty as NonEmpty
import qualified Control.Monad.Combinators.Expr as Expr
import qualified Data.Char as Char
import Data.Foldable (foldr1)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Efg.Syntax as Syntax
import qualified Efg.Type as Type
import qualified Text.Megaparsec as P

type LExpr = Syntax.Expr Offset

sourceBlocks :: Parser [Syntax.SourceBlock Offset]
sourceBlocks = manyTill sourceBlock eof

sourceBlock :: Parser (Syntax.SourceBlock Offset)
sourceBlock = do
  offset <- getOffset
  pos <- getSourcePos
  (src, block) <- withSource $ withRecovery recover sourceBlock'
  return $ Syntax.SourceBlock (unPos (sourceLine pos)) offset src block

recover :: ParseError Text Void -> Parser (Syntax.SourceBlock' Offset)
recover e = do
  pos <- statePosState <$> getParserState
  reachedEOF <-
    try (mayBreak sc >> eof >> return True)
      <|> return False
  consumeTillBreak
  let errorMessage = errorBundlePretty (ParseErrorBundle (e :| []) pos)
  return Syntax.Unparsable {..}

consumeTillBreak :: Parser ()
consumeTillBreak = void $ manyTill anySingle $ eof <|> void (try (eol >> eol))

sourceBlock' :: Parser (Syntax.SourceBlock' Offset)
sourceBlock' =
  optional (try nextLine)
    *> (Syntax.SBTopDecl <$> topDecl)

topDecl :: Parser (Syntax.TopDecl Offset)
topDecl = (topDef <|> topExpr) <* eolf

topExpr :: Parser (Syntax.TopDecl Offset)
topExpr = Syntax.TopExpr <$> expr_

expr_ :: Parser LExpr
expr_ = try annotatedExpr <|> pGroup

annotatedExpr :: Parser LExpr
annotatedExpr = do
  annotated <- pGroup
  sym ":"
  annotation <- quantifiedType
  return Syntax.Annotation {location = Syntax.location annotated, ..}

topDef :: Parser (Syntax.TopDecl Offset)
topDef = do
  keyword "def"
  name <- anyName
  sym "="
  body <- try (withIndent expr_) <|> expr_
  return Syntax.TopDef {..}

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
    'l' -> try (pLet True) <|> pLet False <|> pVariable
    't' -> pBool BITrue <|> pVariable
    'f' -> pBool BIFalse <|> pVariable
    _ -> pVariable

pBool :: BuiltinValue -> Parser LExpr
pBool biv = withOffset \location -> do
  builtinValue biv
  let scalar = Syntax.Bool (biv == BITrue)
  return $ Syntax.Scalar {..}

primType :: Parser (Type.Type Offset)
primType =
  let locatedTyp typ = try (withOffset \location -> do builtinValue typ; return location)
      varType = try (withOffset \location -> do name <- loName; return Type.VariableType {..})
   in ( do location <- locatedTyp BIInteger; return Type.Scalar {scalar = Type.Integer, ..}
          <|> do location <- locatedTyp BIReal; return Type.Scalar {scalar = Type.Real, ..}
          <|> do location <- locatedTyp BIBool; return Type.Scalar {scalar = Type.Bool, ..}
          <|> do location <- locatedTyp BIStr; return Type.Scalar {scalar = Type.Text, ..}
          <|> do varType
          <|> parens quantifiedType
      )

funType :: Parser (Type.Type Offset)
funType =
  try
    do
      let function input output = Type.Function {location = Type.location input, ..}
      ts <- primType `NonEmpty.sepBy1` sym "->"
      return (foldr1 function ts)
    <|> primType

locatedName :: Parser (Offset, Syntax.Name)
locatedName = withOffset \location -> do
  name <- anyName
  return (location, name)

pLam :: Parser LExpr
pLam = mayNotBreak $ withOffset \location -> do
  sym "\\"
  names <- P.some locatedName
  mayNotBreak (sym "->")
  let lamBody = do
        body0 <- expr_
        let cons (nameLocation, name) body = Syntax.Lambda {..}
        return $ foldr cons body0 names
  try (withIndent lamBody) <|> lamBody

pLet :: Bool -> Parser LExpr
pLet isRec = label "let expression" do
  bindings <- NonEmpty.some1 (pBinding isRec <* mayBreak sc)
  void $ optional (keyword KWIn)
  mayBreak sc *> do
    body <- expr_
    return do
      let Syntax.Binding {nameLocation = location} =
            head bindings
      Syntax.Let {..}

pBinding :: Bool -> Parser (Syntax.Binding Offset)
pBinding isRec =
  label "let binding" $
    try (bindingAnn isRec)
      <|> bindingNoAnn isRec

bindingNoAnn :: Bool -> Parser (Syntax.Binding Offset)
bindingNoAnn isRec = do
  keyword KWLet
  when isRec (keyword KWRec)
  (name, (nameOffset, _)) <- withPos loName
  let nameLocation = Offset nameOffset
  sym "="
  mayBreak sc *> do
    assignment <- expr_
    let annotation = Nothing
    return Syntax.Binding {..}

bindingAnn :: Bool -> Parser (Syntax.Binding Offset)
bindingAnn isRec = do
  keyword KWLet
  when isRec (keyword KWRec)
  (name, (nameOffset, _)) <- withPos loName
  let nameLocation = Offset nameOffset
  sym ":"
  annotation <- fmap Just quantifiedType
  sym "="
  mayBreak sc *> do
    mayBreak sc *> do
      assignment <- expr_
      return Syntax.Binding {..}

domain :: Parser Type.Domain
domain = do
  keyword "Type"
  return Type.Type

quantifiedType :: Parser (Type.Type Offset)
quantifiedType = do
  let quantify (forallOrExists, location, (typeVariableOffset, typeVariable), domain_) =
        forallOrExists location typeVariableOffset typeVariable domain_
  fss <-
    P.many
      ( do
          (_, (start, _)) <- withPos (keyword "forall")
          let location = Offset start
          fs <- P.some do
            parens do
              locatedTypeVariable <- withOffset \loc -> do n <- loName; return (loc, n)
              sym ":"
              domain_ <- domain
              return \location_ ->
                quantify (Type.Forall, location_, locatedTypeVariable, domain_)
          sym "."
          return (map ($ location) fs)
      )
  t <- funType
  return (foldr ($) t (concat fss))

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
  name <- anyName
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
