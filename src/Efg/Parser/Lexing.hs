{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Efg.Parser.Lexing where

import Efg.Parser.Location (Location (..), Offset (..))
import Text.Megaparsec hiding (ParseError, parse)
import Text.Megaparsec.Debug

import qualified Data.Char as Char
import qualified Data.HashSet as HS
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Efg.Parser.Location as Location
import qualified Efg.Syntax as Syntax
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

newtype EfgParseError
  = ParsingFailed Location
  deriving stock (Show, Eq)

instance Exception EfgParseError where
  displayException (ParsingFailed loc) =
    toString
      (Location.renderError "Invalid input - Parsing failed" loc)

data ParseCtx = ParseCtx
  { curIndent :: Int
  , canBreak :: Bool
  , prevWhitespace :: Bool
  }

initParseCtx :: ParseCtx
initParseCtx = ParseCtx 0 False False

type Parser = StateT ParseCtx (Parsec Void Text)

parse :: String -> Text -> Parser a -> a
parse filename code p =
  case P.parse (runStateT p initParseCtx) filename code of
    Left err -> error ("This should be unreachable:\n" <> show err)
    Right (x, _) -> x

debug :: (Show a) => String -> Parser a -> Parser a
debug msg p = do
  ctx <- get
  lift $ dbg msg $ fst <$> runStateT p ctx

type Lexer = Parser

sc :: Parser ()
sc = skipSome s <|> pass
  where
    s = hidden space <|> hidden lineComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol s = void (L.symbol sc s)

space :: Parser ()
space =
  gets canBreak >>= \case
    True -> P.space1
    False -> void $ takeWhile1P (Just "white space") (`elem` (" \t" :: String))

lineComment :: Parser ()
lineComment = do
  _ <- try (P.string "--")
  void $ takeWhileP (Just "char") (/= '\n')

nextChar :: Lexer Char
nextChar = do
  i <- getInput
  guard (not (Text.null i))
  return (Text.head i)
{-# INLINE nextChar #-}

data Keyword
  = KWDef
  | KWIf
  | KWThen
  | KWElse
  | KWLet
  | KWIn
  | KWFix
  | KWModule
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance IsString Keyword where
  fromString = \case
    "def" -> KWDef
    "if" -> KWIf
    "then" -> KWThen
    "else" -> KWElse
    "let" -> KWLet
    "in" -> KWIn
    "fix" -> KWFix
    "module" -> KWModule
    _ -> error "Invalid keyword"

keywordToken :: Keyword -> String
keywordToken = \case
  KWDef -> "def"
  KWIf -> "if"
  KWThen -> "then"
  KWElse -> "else"
  KWLet -> "let"
  KWIn -> "in"
  KWFix -> "fix"
  KWModule -> "module"

keyword :: Keyword -> Lexer ()
keyword kw =
  lexeme $
    try $
      P.string (fromString $ keywordToken kw)
        >> notFollowedBy nameTailChar

data BuiltinValue
  = BITrue
  | BIFalse
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance IsString BuiltinValue where
  fromString = \case
    "true" -> BITrue
    "false" -> BIFalse
    _ -> "Invalid builtin-value"

builtinToken :: BuiltinValue -> String
builtinToken = \case
  BITrue -> "true"
  BIFalse -> "false"

builtinValue :: BuiltinValue -> Lexer ()
builtinValue bi =
  lexeme $
    try $
      P.string (fromString $ builtinToken bi)
        >> notFollowedBy nameTailChar

sym :: Text -> Lexer ()
sym s = lexeme $ try $ P.string s >> notFollowedBy symChar

symChar :: Parser Char
symChar = token (\c -> if HS.member c symChars then Just c else Nothing) mempty

symChars :: HashSet Char
symChars = HS.fromList ".,!$^&*:-~+/=<>|?\\@#"

nameTailChar :: Parser Char
nameTailChar = P.alphaNumChar <|> P.char '_' <|> P.char '\''

data NameFirst
  = Upper
  | Lower
  | Anything

nameFirstChar :: NameFirst -> Parser Char
nameFirstChar = \case
  Upper -> P.upperChar
  Lower -> P.lowerChar
  Anything -> P.alphaNumChar

pName :: NameFirst -> Lexer Syntax.Name
pName nf = label "name" $ try $ lexeme do
  c0 <- nameFirstChar nf
  cs <- takeWhileP Nothing (\c -> Char.isAlphaNum c || c == '_' || c == '\'')
  let n = Text.cons c0 cs
  guard (not (isKeyword n))
  return n

loName :: Lexer Syntax.Name
loName = pName Lower

upName :: Lexer Syntax.Name
upName = pName Upper

anyName :: Lexer Syntax.Name
anyName = pName Anything

isKeyword :: Text -> Bool
isKeyword candidate = candidate `HS.member` keywordSet

keywordSet :: HashSet Text
keywordSet = HS.fromList keywordStrings

keywordStrings :: [Text]
keywordStrings = map (fromString . keywordToken) universe

charLit :: Lexer Char
charLit = lexeme $ P.char '\'' >> L.charLiteral <* P.char '\''

strLit :: Lexer String
strLit = lexeme $ P.char '"' >> manyTill L.charLiteral (P.char '"')

natLit :: Lexer Integer
natLit = lexeme $ try $ L.decimal <* notFollowedBy (P.char '.')

doubleLit :: Lexer Double
doubleLit =
  lexeme $
    try L.float
      <|> try (fromIntegral <$> (L.decimal :: Parser Int) <* P.char '.')
      <|> try do
        s <- L.scientific
        case Scientific.toBoundedRealFloat s of
          Right f -> return f
          Left _ -> fail "Non-representable floating point literal"

lParen :: Lexer ()
lParen = symbol "("

rParen :: Lexer ()
rParen = symbol ")"

lBrace :: Lexer ()
lBrace = symbol "{"

rBrace :: Lexer ()
rBrace = symbol "}"

lBracket :: Lexer ()
lBracket = symbol "["

rBracket :: Lexer ()
rBracket = symbol "]"

pLocal :: (ParseCtx -> ParseCtx) -> Parser a -> Parser a
pLocal f p = do
  s <- get
  put (f s) >> p <* put s
{-# INLINE pLocal #-}

mayBreak :: Parser a -> Parser a
mayBreak = pLocal (\ctx -> ctx {canBreak = True})
{-# INLINE mayBreak #-}

mayNotBreak :: Parser a -> Parser a
mayNotBreak = pLocal (\ctx -> ctx {canBreak = False})
{-# INLINE mayNotBreak #-}

precededByWhitespace :: Parser Bool
precededByWhitespace = gets prevWhitespace
{-# INLINE precededByWhitespace #-}

recordNonWhitespace :: Parser ()
recordNonWhitespace = modify (\ctx -> ctx {prevWhitespace = False})
{-# INLINE recordNonWhitespace #-}

recordWhitespace :: Parser ()
recordWhitespace = modify (\ctx -> ctx {prevWhitespace = True})
{-# INLINE recordWhitespace #-}

bracketed :: Parser () -> Parser () -> Parser a -> Parser a
bracketed l r p = between l r $ mayBreak (sc >> p)
{-# INLINE bracketed #-}

parens :: Parser a -> Parser a
parens = bracketed lParen rParen
{-# INLINE parens #-}

brackets :: Parser a -> Parser a
brackets = bracketed lBracket rBracket
{-# INLINE brackets #-}

braces :: Parser a -> Parser a
braces = bracketed lBrace rBrace
{-# INLINE braces #-}

withOffset :: (Offset -> Parser a) -> Parser a
withOffset p = do
  o <- getOffset
  p (Offset o)

withPos :: Parser a -> Parser (a, (Int, Int))
withPos p = do
  n <- getOffset
  x <- p
  n' <- getOffset
  return (x, (n, n'))
{-# INLINE withPos #-}

nextLine :: Parser ()
nextLine = do
  eol
  n <- curIndent <$> get
  void $ mayNotBreak $ P.many $ try (sc >> eol)
  replicateM_ n (P.char ' ')

withIndent :: Parser a -> Parser a
withIndent p = do
  nextLine
  indent <- Text.length <$> takeWhileP (Just "space") (== ' ')
  when (indent <= 0) empty
  pLocal (\ctx -> ctx {curIndent = curIndent ctx + indent}) $ mayNotBreak p
{-# INLINE withIndent #-}

withSource :: Parser a -> Parser (Text, a)
withSource p = do
  s <- getInput
  (x, (start, end)) <- withPos p
  return (Text.take (end - start) s, x)
{-# INLINE withSource #-}

eol :: Parser ()
eol = void P.eol

eolf :: Parser ()
eolf = void P.eof <|> eol
