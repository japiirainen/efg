module Efg where

-- import Control.Monad.State.Strict (StateT, runStateT)
import Text.Megaparsec (Parsec)

import qualified Text.Megaparsec as Megaparsec

data ParseCtx = ParseCtx
  { curIndent :: Int
  , canBreak :: Bool
  , prevWhitespace :: Bool
  }

initParseCtx :: ParseCtx
initParseCtx = ParseCtx 0 False False

type Parser = StateT ParseCtx (Parsec Void Text)

parseit :: Text -> Parser a -> Either () a
parseit s p = case Megaparsec.parse (fst <$> runStateT p initParseCtx) "" s of
  Left e -> undefined
  Right x -> pure x

main :: IO ()
main = do
  putStrLn "Hello, world!"
