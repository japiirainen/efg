module Efg.Parser.Location (
  Location (..),
  Offset (..),
  renderError,
) where

import Text.Megaparsec (PosState (..), SourcePos (..))

import qualified Data.Text as Text
import qualified Text.Megaparsec.Pos as Pos
import qualified Text.Megaparsec.Stream as Stream

newtype Offset = Offset {unOffset :: Int}
  deriving newtype (Eq, Num, Show)

data Location = Location
  { filename :: String
  , code :: Text
  , offset :: Offset
  }
  deriving stock (Eq, Show)

renderError :: Text -> Location -> Text
renderError message Location {..} = prefix <> "\n" <> suffix
  where
    initialState =
      PosState
        { pstateInput = code
        , pstateOffset = 0
        , pstateSourcePos = Pos.initialPos filename
        , pstateTabWidth = Pos.defaultTabWidth
        , pstateLinePrefix = ""
        }

    (h, state') = Stream.reachOffset (unOffset offset) initialState

    pos = pstateSourcePos state'

    line = Pos.unPos (sourceLine pos)

    column = Pos.unPos (sourceColumn pos)

    suffix = case h of
      Just string ->
        let lineText = show line

            inner = lineText <> " │"

            outer = Text.replicate (Text.length lineText) " " <> " │"

            caret = Text.replicate (column - 1) " " <> "↑"
         in outer
              <> "\n\
                 \"
              <> inner
              <> " "
              <> toText string
              <> "\n\
                 \"
              <> outer
              <> " "
              <> caret
      Nothing ->
        ""

    prefix =
      toText filename
        <> ":"
        <> show line
        <> ":"
        <> show column
        <> ": "
        <> message
