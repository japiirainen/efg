module Efg.Parser (
  parseModule,
) where

import Efg.Parser.Lexing (parse)
import Efg.Parser.Location (Offset)

import qualified Efg.Parser.Parsing as Parsing
import qualified Efg.Syntax as Syntax

parseModule :: String -> Text -> Syntax.Module Offset
parseModule filename code = do
  let blocks = parse filename code Parsing.sourceBlocks
  Syntax.Module (toText filename) blocks
