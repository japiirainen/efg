module Efg.Parser.Lexer (
  Token (..),
  LocatedToken (..),
  lex,
  reserved,
  ParseError (..),
) where

import Control.Monad.Combinators (manyTill)
import Data.Scientific (Scientific)

import Efg.Parser.Location (Location (..), Offset (..))
import Text.Megaparsec (ParseErrorBundle (..), try, (<?>))

import qualified Control.Monad.Combinators as Combinators
import qualified Data.Char as Char
import qualified Data.HashSet as HashSet
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import qualified Efg.Parser.Location as Location
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as Error

type Lexer = Megaparsec.Parsec Void Text

space :: Lexer ()
space = L.space Megaparsec.Char.space1 (L.skipLineComment "#") empty

symbol :: Text -> Lexer Text
symbol = L.symbol space

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme space

parseToken :: Lexer Token
parseToken =
  Combinators.choice
    [ label
    , Combinators.choice
        [ Or <$ symbol "||"
        , And <$ symbol "&&"
        , Plus <$ symbol "+"
        , Times <$ symbol "*"
        ]
        <?> "operator"
    , OpenAngle <$ symbol "<"
    , CloseAngle <$ symbol ">"
    , OpenParen <$ symbol "("
    , CloseParen <$ symbol ")"
    , OpenBracket <$ symbol "["
    , CloseBracket <$ symbol "]"
    , OpenBrace <$ symbol "{"
    , CloseBrace <$ symbol "}"
    , Dash <$ symbol "-"
    , number
    , text
    ]

parseLocatedToken :: Lexer LocatedToken
parseLocatedToken = do
  start <- Offset <$> Megaparsec.getOffset
  token <- parseToken
  pure LocatedToken {..}

parseLocatedTokens :: Lexer [LocatedToken]
parseLocatedTokens = space >> manyTill parseLocatedToken Megaparsec.eof

lex :: String -> Text -> Either ParseError [LocatedToken]
lex filename code = case Megaparsec.parse parseLocatedTokens filename code of
  Left ParseErrorBundle {..} -> do
    let bundleError :| _ = bundleErrors
    let offset = Offset (Error.errorOffset bundleError)
    Left (LexingFailed (Location {..}))
  Right tokens -> pure tokens

number :: Lexer Token
number = do
  scientific <- lexeme L.scientific
  case Scientific.toBoundedInteger scientific of
    Nothing -> pure (RealLit scientific)
    Just int -> pure (Int int)

text :: Lexer Token
text = lexeme do
  _ <- "\""
  let isText c =
        ('\x20' <= c && c <= '\x21')
          || ('\x23' <= c && c <= '\x5b')
          || ('\x5d' <= c && c <= '\x10FFFF')

  let unescaped = Megaparsec.takeWhile1P (Just "text character") isText

  let unicodeEscape = do
        _ <- "\\u"

        codepoint <- Combinators.count 4 Megaparsec.Char.hexDigitChar

        case Read.hexadecimal (toText codepoint) of
          Right (n, "") -> do
            return (one (chr n))
          _ -> do
            fail "Internal error - invalid unicode escape sequence"

  let escaped =
        Combinators.choice
          [ "\"" <$ "\\\""
          , "\\" <$ "\\\\"
          , "/" <$ "\\/"
          , "\b" <$ "\\b"
          , "\f" <$ "\\f"
          , "\n" <$ "\\n"
          , "\r" <$ "\\r"
          , "\t" <$ "\\t"
          , unicodeEscape
          ]
          <?> "escape sequence"

  texts <- many (unescaped <|> escaped)

  _ <- "\""

  return (TextLit (Text.concat texts))

isLabel0 :: Char -> Bool
isLabel0 c = Char.isLower c || c == '_'

isLabel :: Char -> Bool
isLabel c = Char.isAlphaNum c || c == '_' || c == '-' || c == '/'

reserved :: HashSet Text
reserved = HashSet.fromList []

label :: Lexer Token
label = (lexeme . try) do
  c0 <- Megaparsec.satisfy isLabel0 <?> "label character"
  cs <- Megaparsec.takeWhileP (Just "label character") isLabel
  let t = Text.cons c0 cs
  guard (not (HashSet.member t reserved))
  return (Label t)

data Token
  = Int Int
  | RealLit Scientific
  | TextLit Text
  | Label Text
  | Plus
  | Times
  | Dash
  | And
  | Or
  | OpenAngle
  | CloseAngle
  | OpenBrace
  | CloseBrace
  | OpenBracket
  | CloseBracket
  | OpenParen
  | CloseParen
  deriving stock (Eq, Show)

data LocatedToken = LocatedToken
  { token :: Token
  , start :: Offset
  }
  deriving stock (Show)

data ParseError
  = LexingFailed Location
  | ParsingFailed Location
  deriving stock (Eq, Show)

instance Exception ParseError where
  displayException (LexingFailed location) =
    toString
      (Location.renderError "Invalid input - Lexing failed" location)
  displayException (ParsingFailed location) =
    toString
      (Location.renderError "Invalid input - Parsing failed" location)
