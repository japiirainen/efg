{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Efg.Type (
  Type (..),
  Monotype (..),
  prettyTextLiteral,
) where

import Efg.Pretty (Pretty (..), builtin, label, punctuation)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prelude hiding (Type)

import qualified Data.Text as Text
import qualified Prettyprinter as Pretty

-- Monomorphic types

data Monotype
  = MVariableType Text
  | MFunction Monotype Monotype
  | MScalar Scalar
  deriving stock (Eq, Generic, Show)

instance IsString Monotype where
  fromString string = MVariableType (fromString string)

data Scalar
  = Bool
  | Real
  | Integer
  | Natural
  | Text
  deriving stock (Eq, Generic, Lift, Show)

instance Pretty Scalar where
  pretty Bool = builtin "Bool"
  pretty Real = builtin "Real"
  pretty Integer = builtin "Integer"
  pretty Natural = builtin "Natural"
  pretty Text = builtin "Text"

-- Polymorphic types

type TypeName = Text

-- | @Type@ is a potentially polymorphic type.
data Type loc
  = VariableType {location :: loc, name :: TypeName}
  | Function {location :: loc, input :: Type loc, output :: Type loc}
  | Scalar {location :: loc, scalar :: Scalar}
  deriving stock (Eq, Functor, Generic, Lift, Show, Foldable, Traversable)

instance IsString (Type ()) where
  fromString s = VariableType {location = (), name = fromString s}

instance Pretty (Type s) where
  pretty = prettyType

prettyType :: Type s -> Doc AnsiStyle
prettyType t@Function {} = Pretty.group (Pretty.flatAlt long short)
  where
    long = Pretty.align (prettyLong t)

    short = prettyShort t

    prettyShort Function {..} =
      prettyPrimitiveType input
        <> " "
        <> punctuation "->"
        <> " "
        <> prettyShort output
    prettyShort _A =
      prettyPrimitiveType _A

    prettyLong Function {..} =
      prettyPrimitiveType input
        <> " "
        <> punctuation "->"
        <> Pretty.hardline
        <> prettyLong output
    prettyLong _A =
      "  " <> prettyPrimitiveType _A
prettyType other = prettyPrimitiveType other

prettyPrimitiveType :: Type s -> Doc AnsiStyle
prettyPrimitiveType VariableType {..} = label (pretty name)
prettyPrimitiveType Scalar {..} = pretty scalar
prettyPrimitiveType other = Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "(" <> prettyType other <> punctuation ")"
    long =
      Pretty.align
        ( punctuation "("
            <> " "
            <> prettyType other
            <> Pretty.hardline
            <> punctuation ")"
        )

-- | Pretty-print a @Text@ literal
prettyTextLiteral :: Text -> Doc AnsiStyle
prettyTextLiteral text =
  "\""
    <> ( pretty
          . Text.replace "\"" "\\\""
          . Text.replace "\b" "\\b"
          . Text.replace "\f" "\\f"
          . Text.replace "\n" "\\n"
          . Text.replace "\r" "\\r"
          . Text.replace "\t" "\\t"
          . Text.replace "\\" "\\\\"
       )
      text
    <> "\""