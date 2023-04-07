module Efg.Parser.Lexing where

import Data.Char (isAlpha, isAlphaNum)
import Efg.Errors (ErrType (..), Except (..), SrcPos)
import Text.Megaparsec (Parsec)

import qualified Data.Scientific as Scientific
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Efg.Errors as Errors
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Debug as Megaparsec

data ParseCtx = ParseCtx
  { curIndent :: Int
  , canBreak :: Bool
  , prevWhitespace :: Bool
  }

initParseCtx :: ParseCtx
initParseCtx = ParseCtx 0 False False

type Parser = StateT ParseCtx (Parsec Void Text)

runParser ::
  String ->
  -- | Filename
  Text ->
  -- | Input string
  Parser a ->
  -- | Parser to run
  Except a
runParser filename s p = case Megaparsec.parse (fst <$> runStateT p initParseCtx) filename s of
  Left e -> Errors.throw ParseError $ Megaparsec.errorBundlePretty e
  Right x -> pure x

runParserUnsafe ::
  String ->
  -- | Filename
  Text ->
  -- | Input string
  Parser a ->
  -- | Parser to run
  a
runParserUnsafe filename s p = case runParser filename s p of
  Failure err -> error ("This should never happen: " <> show err)
  Success x -> x

debugParser :: (Show a) => String -> Parser a -> Parser a
debugParser label p = do
  ctx <- get
  lift $ Megaparsec.dbg label (fmap fst (runStateT p ctx))

recordWhitespace :: Parser ()
recordWhitespace = do
  ctx <- get
  put ctx {prevWhitespace = True}

recordNonWhitespace :: Parser ()
recordNonWhitespace = do
  ctx <- get
  put ctx {prevWhitespace = False}

pLocal :: (ParseCtx -> ParseCtx) -> Parser a -> Parser a
pLocal f p = do
  ctx <- get
  put (f ctx)
  a <- p
  put ctx
  pure a

type Lexer = Parser

space :: Parser ()
space =
  gets canBreak >>= \case
    True -> Megaparsec.space1
    False -> void $ Megaparsec.takeWhile1P (Just "whitespace") (\c -> c == ' ' || c == '\t')

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

sc :: Parser ()
sc = (Megaparsec.skipSome s >> recordWhitespace) <|> pass
  where
    s = Megaparsec.hidden space <|> Megaparsec.hidden lineComment

lexeme :: Lexer a -> Lexer a
lexeme p = L.lexeme sc (p <* recordNonWhitespace)

symbol :: Text -> Lexer ()
symbol s = void $ L.symbol sc s

eol :: Parser ()
eol = void Megaparsec.eol

eolf :: Parser ()
eolf = void Megaparsec.eof <|> eol

nextChar :: Lexer Char
nextChar = do
  i <- Megaparsec.getInput
  guard (not (Text.null i))
  return (Text.head i)

anyName :: Lexer Text
anyName = anyCaseName <|> symName

anyCaseName :: Lexer Text
anyCaseName =
  Megaparsec.label "name" $
    lexeme $
      notKeyword $
        Text.cons
          <$> Megaparsec.satisfy isAlpha
          <*> Megaparsec.takeWhileP Nothing (\c -> isAlphaNum c || c == '\'' || c == '_')

keywords :: [Text]
keywords =
  [ "let"
  , "in"
  , "of"
  , "if"
  , "then"
  , "else"
  , "def"
  ]

keywordsSet :: Set Text
keywordsSet = Set.fromList keywords

keyword :: Text -> Lexer ()
keyword kw =
  lexeme $
    Megaparsec.try $
      Megaparsec.string kw
        >> Megaparsec.notFollowedBy nameTailChar

nameTailChar :: Lexer Char
nameTailChar = Megaparsec.alphaNumChar <|> Megaparsec.char '\'' <|> Megaparsec.char '_'

notKeyword :: Lexer Text -> Lexer Text
notKeyword p = Megaparsec.try do
  name <- p
  failIf (Set.member name keywordsSet) ("Keyword " <> show name <> " cannot be used as an identifier")
  return name

charLit :: Lexer Char
charLit =
  lexeme $
    Megaparsec.char '\''
      >> L.charLiteral
        <* Megaparsec.char '\''

stringLit :: Lexer Text
stringLit = lexeme $ do
  _ <- Megaparsec.char '"'
  s <- Megaparsec.takeWhileP Nothing (/= '"')
  _ <- Megaparsec.char '"'
  pure s

natLit :: Lexer Word64
natLit = lexeme $ Megaparsec.try $ L.decimal <* Megaparsec.notFollowedBy (Megaparsec.char '.')

intLit :: Lexer Int
intLit = fromIntegral <$> (natLit <|> (negate <$> lexeme (Megaparsec.char '-' *> natLit)))

doubleLit :: Lexer Double
doubleLit =
  lexeme $
    Megaparsec.try L.float
      <|> Megaparsec.try (fromIntegral <$> (L.decimal :: Lexer Int)) <* Megaparsec.char '.'
      <|> Megaparsec.try do
        s <- L.scientific
        case Scientific.toBoundedRealFloat s of
          Right x -> pure x
          Left _ -> fail "Non-representable floating point literal"

knownSymbols :: Set String
knownSymbols =
  Set.fromList
    ["+", "-", "*", "/", "^", "!", ":", "=", "<", ">", "<=", ">=", "&&", "||"]

sym :: Text -> Lexer ()
sym s =
  lexeme $
    Megaparsec.try $
      Megaparsec.string s
        >> Megaparsec.notFollowedBy symChar

anySym :: Lexer Text
anySym = lexeme $ Megaparsec.try $ do
  s <- Megaparsec.some symChar
  failIf (s `Set.member` knownSymbols) ""
  pure (toText s)

symName :: Lexer Text
symName = Megaparsec.label "symbol name" $ lexeme $ Megaparsec.try $ do
  s <- Megaparsec.between (Megaparsec.char '(') (Megaparsec.char ')') (Megaparsec.some symChar)
  return $ "(" <> toText s <> ")"

symChar :: Lexer Char
symChar =
  Megaparsec.token
    (\c -> if c `Set.member` symChars then Just c else Nothing)
    mempty

outputLines :: Parser ()
outputLines = void $ many (symbol ">" >> Megaparsec.takeWhileP Nothing (/= '\n') >> (eol >> pass <|> Megaparsec.eof))

lparen :: Lexer ()
lparen = charLexeme '('

rparen :: Lexer ()
rparen = charLexeme ')'

lbrace :: Lexer ()
lbrace = charLexeme '{'

rbrace :: Lexer ()
rbrace = charLexeme '}'

lbracket :: Lexer ()
lbracket = charLexeme '['

rbracket :: Lexer ()
rbracket = charLexeme ']'

semicolon :: Lexer ()
semicolon = charLexeme ';'

underscore :: Lexer ()
underscore = charLexeme '_'

charLexeme :: Char -> Lexer ()
charLexeme c = void $ lexeme $ Megaparsec.char c

symChars :: Set Char
symChars = Set.fromList "+-*/^!:=<>&|"

mayBreak :: Parser a -> Parser a
mayBreak = pLocal (\ctx -> ctx {canBreak = True})

mayNotBreak :: Parser a -> Parser a
mayNotBreak = pLocal (\ctx -> ctx {canBreak = False})

precededByWhitespace :: Parser Bool
precededByWhitespace = gets prevWhitespace

nameString :: Parser Text
nameString =
  lexeme . Megaparsec.try $
    Text.cons
      <$> Megaparsec.lowerChar
      <*> (toText <$> many Megaparsec.alphaNumChar)

thisNameString :: Text -> Parser ()
thisNameString s = lexeme $ Megaparsec.try $ do
  _ <- Megaparsec.string s
  Megaparsec.notFollowedBy Megaparsec.alphaNumChar

bracketed :: Parser () -> Parser () -> Parser a -> Parser a
bracketed l r p = Megaparsec.between l r $ mayBreak (sc >> p)

parens :: Parser a -> Parser a
parens = bracketed lparen rparen

brackets :: Parser a -> Parser a
brackets = bracketed lbracket rbracket

braces :: Parser a -> Parser a
braces = bracketed lbrace rbrace

withPos :: Parser a -> Parser (a, SrcPos)
withPos p = do
  n <- Megaparsec.getOffset
  x <- p
  n' <- Megaparsec.getOffset
  return (x, (n, n'))

nextLine :: Parser ()
nextLine = do
  eol
  n <- gets curIndent
  void $ mayNotBreak $ many $ Megaparsec.try (sc >> eol)
  replicateM_ n (Megaparsec.char ' ')

withSource :: Parser a -> Parser (Text, a)
withSource p = do
  i <- Megaparsec.getInput
  (x, (s, e)) <- withPos p
  pure (Text.take (e - s) i, x)

withIndent :: Parser a -> Parser a
withIndent p = do
  nextLine
  indent <- Text.length <$> Megaparsec.takeWhileP (Just "space") (== ' ')
  when (indent <= 0) empty
  pLocal (\ctx -> ctx {curIndent = curIndent ctx + indent}) p

failIf :: Bool -> String -> Parser ()
failIf b msg = guard (not b) <|> fail msg
