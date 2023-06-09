{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Efg.Pretty (
  renderStrict,
  renderIO,
  toText,
  Pretty (..),
  keyword,
  punctuation,
  label,
  scalar,
  builtin,
  operator,
) where

import Data.Scientific (Scientific)
import Prettyprinter (Doc, LayoutOptions (..), PageWidth (..))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prelude hiding (toText)

import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.Terminal as Pretty.Terminal
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified Text.URI as URI

renderStrict ::
  Pretty a =>
  -- | `True` enable syntax highlighting
  Bool ->
  -- | Available columns
  Int ->
  a ->
  Text
renderStrict highlight columns =
  render . Pretty.layoutSmart (layoutOptions columns) . pretty
  where
    render =
      if highlight
        then Pretty.Terminal.renderStrict
        else Pretty.Text.renderStrict

renderIO ::
  Pretty a =>
  -- | `True` enables syntax highlighting
  Bool ->
  -- | Available columns
  Int ->
  Handle ->
  a ->
  IO ()
renderIO highlight columns handle =
  render handle . Pretty.layoutSmart (layoutOptions columns) . pretty
  where
    render =
      if highlight
        then Pretty.Terminal.renderIO
        else Pretty.Text.renderIO

-- | Simple conversion of a document to `Text`
toText :: Pretty a => a -> Text
toText = Pretty.Text.renderStrict . Pretty.layoutCompact . pretty

{- | This is like @"Prettyprinter".`Prettyprinter.Pretty`@, except that this
    can return a `Doc` with `AnsiStyle` annotations
-}
class Pretty a where
  pretty :: a -> Doc AnsiStyle

instance Pretty Double where
  pretty = Pretty.pretty

instance Pretty Scientific where
  pretty = Pretty.pretty . show @String

instance Pretty Int where
  pretty = Pretty.pretty

instance Pretty Integer where
  pretty = Pretty.pretty

instance Pretty Natural where
  pretty = Pretty.pretty

instance Pretty Text where
  pretty = Pretty.pretty

instance Pretty () where
  pretty = Pretty.pretty

instance Pretty Void where
  pretty = Pretty.pretty

instance Pretty String where
  pretty = Pretty.pretty

instance Pretty URI.URI where
  pretty = Pretty.pretty . URI.render

instance Pretty (Doc AnsiStyle) where
  pretty = id

layoutOptions ::
  -- | Available columns
  Int ->
  LayoutOptions
layoutOptions columns =
  LayoutOptions {layoutPageWidth = AvailablePerLine columns 1}

-- | Highlight a keyword (e.g. @let@ or @merge@)
keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword =
  Pretty.annotate
    ( Pretty.Terminal.bold
        <> Pretty.Terminal.colorDull Pretty.Terminal.Green
    )

-- | Highlight punctuation (e.g. @{@ or @,@)
punctuation :: Doc AnsiStyle -> Doc AnsiStyle
punctuation =
  Pretty.annotate
    ( Pretty.Terminal.bold
        <> Pretty.Terminal.colorDull Pretty.Terminal.Green
    )

-- | Highlight a label (e.g. @x@)
label :: Doc AnsiStyle -> Doc AnsiStyle
label = Pretty.annotate mempty

-- | Highlight a scalar (e.g. @1@ or @\"abc\"@)
scalar :: Doc AnsiStyle -> Doc AnsiStyle
scalar = Pretty.annotate (Pretty.Terminal.colorDull Pretty.Terminal.Magenta)

-- | Highlight a built-in (e.g. @List/length@)
builtin :: Doc AnsiStyle -> Doc AnsiStyle
builtin = Pretty.annotate Pretty.Terminal.underlined

-- | Highlight an operator (e.g. @+@ or @&&@)
operator :: Doc AnsiStyle -> Doc AnsiStyle
operator =
  Pretty.annotate
    ( Pretty.Terminal.bold
        <> Pretty.Terminal.colorDull Pretty.Terminal.Green
    )
