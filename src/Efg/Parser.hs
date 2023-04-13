module Efg.Parser (
  parseModule,
) where

import Efg.Parser.Lexing (parse)
import Efg.Parser.Location (Offset)
import Efg.Parser.Parsing (sourceBlocks)

import qualified Efg.Syntax as Syntax

parseModule :: String -> Text -> Syntax.Module Offset
parseModule filename code = do
  let blocks = parse filename code sourceBlocks
  Syntax.Module (toText filename) blocks
