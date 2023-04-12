module Efg.Parser.Lexing where

import Efg.Parser.Location (Location (..))
import Text.Megaparsec hiding (ParseError, parse)

import qualified Efg.Parser.Location as Location
import qualified Text.Megaparsec as Megaparsec

newtype ParseError
  = ParsingFailed Location
  deriving stock (Show, Eq)

instance Exception ParseError where
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

-- parse :: String -> Text -> Parser a -> Either ParseError a
-- parse filename input p = case Megaparsec.parse (fst <$> runStateT p initParseCtx) filename s of
--   Left e -> do undefined
--   Right x -> Right x
