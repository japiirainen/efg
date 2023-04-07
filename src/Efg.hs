module Efg where

import Efg.Parser
import Efg.Syntax (ModuleSrcName (User))

main :: IO ()
main = do
  readFileText "t.efg" >>= print . parseUModule (User "t.efg")