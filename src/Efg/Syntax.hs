{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Efg.Syntax (
  Module (..),
  moduleErrors,
  moduleTopDecls,
  SourceBlock (..),
  SourceBlock' (..),
  TopDecl (..),
  Expr (..),
  Scalar (..),
  Operator (..),
  Builtin (..),
  Binding (..),
  Name,
) where

import Efg.Type (Type)
import Prelude hiding (Type)

import Efg.Pretty (Pretty (..), keyword, label, punctuation)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Data.Text as Text
import qualified Efg.Pretty as Pretty
import qualified Efg.Type as Type
import qualified Prettyprinter as Pretty

type Name = Text

type ReachedEOF = Bool

data Module loc = Module
  { moduleName :: Text
  , moduleSourceBlocks :: [SourceBlock loc]
  }
  deriving stock (Show, Generic)

moduleErrors :: Module loc -> [String]
moduleErrors Module {moduleSourceBlocks} =
  [ sbText
  | SourceBlock {sbContents = Unparsable _ sbText, sbLine = _, sbOffset = _} <- moduleSourceBlocks
  ]

moduleTopDecls :: Module loc -> [TopDecl loc]
moduleTopDecls Module {moduleSourceBlocks} =
  [ sbContents
  | SourceBlock {sbContents = SBTopDecl sbContents, sbLine = _, sbOffset = _} <- moduleSourceBlocks
  ]

data SourceBlock loc = SourceBlock
  { sbLine :: Int
  , sbOffset :: Int
  , sbText :: Text
  , sbContents :: SourceBlock' loc
  }
  deriving stock (Show, Generic)

data SourceBlock' loc
  = SBTopDecl (TopDecl loc)
  | Unparsable ReachedEOF String
  deriving stock (Show, Generic)

data TopDecl loc
  = TopExpr {expr :: Expr loc}
  | TopDef {name :: Name, body :: Expr loc}
  deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance (Pretty a) => Pretty (TopDecl a) where
  pretty = prettyTopDecl

data Expr loc
  = Variable {location :: loc, name :: Name}
  | Scalar {location :: loc, scalar :: Scalar}
  | Operator {location :: loc, left :: Expr loc, operatorLocation :: loc, operator :: Operator, right :: Expr loc}
  | If {location :: loc, predicate :: Expr loc, ifTrue :: Expr loc, ifFalse :: Expr loc}
  | Lambda {location :: loc, nameLocation :: loc, name :: Name, body :: Expr loc}
  | Application {location :: loc, function :: Expr loc, argument :: Expr loc}
  | Let {location :: loc, bindings :: NonEmpty (Binding loc), body :: Expr loc}
  | Fix {location :: loc, nameLocation :: loc, name :: Name, body :: Expr loc}
  | Builtin {location :: loc, builtin :: Builtin}
  deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance (Pretty a) => Pretty (Expr a) where
  pretty = prettyExpression

data Operator
  = And
  | Or
  | Plus
  | Times
  deriving stock (Eq, Generic, Lift, Show)

instance IsString Operator where
  fromString "&&" = And
  fromString "||" = Or
  fromString "+" = Plus
  fromString "*" = Times
  fromString _ = error "unreachable"

instance Pretty Operator where
  pretty And = Pretty.operator "&&"
  pretty Or = Pretty.operator "||"
  pretty Plus = Pretty.operator "+"
  pretty Times = Pretty.operator "*"

data Scalar
  = Real Double
  | Integer Integer
  | Natural Natural
  | Bool Bool
  | Text Text
  deriving stock (Eq, Generic, Lift, Show)

instance Pretty Scalar where
  pretty (Bool True) = Pretty.scalar "true"
  pretty (Bool False) = Pretty.scalar "false"
  pretty (Real n) = Pretty.scalar (pretty n)
  pretty (Integer n) = Pretty.scalar (pretty n)
  pretty (Natural n) = Pretty.scalar (pretty n)
  pretty (Text t) = Pretty.scalar (Type.prettyTextLiteral t)

data Builtin
  = RealEqual
  deriving stock (Eq, Generic, Lift, Show)

instance Pretty Builtin where
  pretty RealEqual = Pretty.builtin "Real/equal"

{--| The assignment part of a @let@ binding

     let x = y
     let x : X = y
-}
data Binding loc = Binding
  { nameLocation :: loc
  , name :: Text
  , annotation :: Maybe (Type loc)
  , assignment :: Expr loc
  }
  deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Pretty a => Pretty (Binding a) where
  pretty Binding {annotation = Nothing, ..} =
    Pretty.group (Pretty.flatAlt long short)
    where
      long =
        Pretty.align
          ( keyword "let"
              <> " "
              <> label (pretty name)
              <> Pretty.hardline
              <> "      "
              <> punctuation "="
              <> " "
              <> pretty assignment
          )

      short =
        keyword "let"
          <> " "
          <> label (pretty name)
          <> " "
          <> punctuation "="
          <> " "
          <> pretty assignment
  pretty Binding {annotation = Just type_, ..} =
    Pretty.group (Pretty.flatAlt long short)
    where
      long =
        Pretty.align
          ( keyword "let"
              <> " "
              <> label (pretty name)
              <> Pretty.hardline
              <> "      "
              <> Pretty.operator ":"
              <> " "
              <> pretty type_
              <> Pretty.hardline
              <> "      "
              <> punctuation "="
              <> " "
              <> pretty assignment
          )
      short =
        keyword "let"
          <> " "
          <> label (pretty name)
          <> " "
          <> Pretty.operator ":"
          <> " "
          <> pretty type_
          <> " "
          <> punctuation "="
          <> " "
          <> pretty assignment

prettyTopDecl :: Pretty a => TopDecl a -> Doc AnsiStyle
prettyTopDecl (TopExpr expr) = prettyExpression expr
prettyTopDecl TopDef {..} =
  Pretty.group (Pretty.flatAlt long short)
  where
    short = keyword "def" <> " " <> label (pretty name) <> " " <> punctuation "=" <> " " <> pretty body
    long =
      Pretty.align
        ( keyword "def"
            <> " "
            <> label (pretty name)
            <> Pretty.hardline
            <> "  "
            <> punctuation "="
            <> " "
            <> pretty body
        )

prettyExpression :: Pretty a => Expr a -> Doc AnsiStyle
prettyExpression expr@Lambda {} =
  Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "\\" <> prettyShort expr
    long = Pretty.align (prettyLong expr)
    prettyShort Lambda {..} = label (pretty name) <> prettyShort body
    prettyShort other = punctuation "." <> " " <> prettyExpression other
    prettyLong Lambda {..} =
      punctuation "\\"
        <> label (pretty name)
        <> Pretty.punctuation "."
        <> Pretty.hardline
        <> prettyLong body
    prettyLong other = "  " <> prettyExpression other
prettyExpression Let {..} = Pretty.group (Pretty.flatAlt long short)
  where
    short =
      foldMap (\binding -> pretty binding <> " ") bindings
        <> keyword "in"
        <> " "
        <> prettyExpression body

    long =
      Pretty.align
        ( foldMap (\binding -> pretty binding <> Pretty.hardline <> Pretty.hardline) bindings
            <> keyword "in"
            <> "  "
            <> prettyExpression body
        )
prettyExpression If {..} =
  Pretty.group (Pretty.flatAlt long short)
  where
    short =
      keyword "if"
        <> " "
        <> prettyExpression predicate
        <> " "
        <> keyword "then"
        <> " "
        <> prettyExpression ifTrue
        <> " "
        <> keyword "else"
        <> " "
        <> prettyExpression ifFalse

    long =
      Pretty.align
        ( keyword "if"
            <> " "
            <> prettyExpression predicate
            <> Pretty.hardline
            <> keyword "then"
            <> " "
            <> prettyExpression ifTrue
            <> Pretty.hardline
            <> keyword "else"
            <> " "
            <> prettyExpression ifFalse
        )
prettyExpression other = prettyTimesExpression other

prettyOperator ::
  Pretty loc =>
  Operator ->
  (Expr loc -> Doc AnsiStyle) ->
  (Expr loc -> Doc AnsiStyle)
prettyOperator operator0 prettyNext expression@Operator {operator = operator1}
  | operator0 == operator1 = Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort Operator {..}
      | operator0 == operator =
          prettyShort left
            <> " "
            <> pretty operator
            <> " "
            <> prettyNext right
    prettyShort other =
      prettyNext other

    prettyLong Operator {..}
      | operator0 == operator =
          prettyLong left
            <> Pretty.hardline
            <> pretty operator
            <> pretty (Text.replicate spacing " ")
            <> prettyNext right
    prettyLong other =
      pretty (Text.replicate indent " ")
        <> prettyNext other

    operatorWidth = Text.length (Pretty.toText operator0)

    alignment = 2

    align n = ((n `div` alignment) + 1) * alignment

    indent = align operatorWidth

    spacing = indent - operatorWidth
prettyOperator _ prettyNext other =
  prettyNext other

prettyTimesExpression :: Pretty loc => Expr loc -> Doc AnsiStyle
prettyTimesExpression = prettyOperator Times prettyPlusExpression

prettyPlusExpression :: Pretty loc => Expr loc -> Doc AnsiStyle
prettyPlusExpression = prettyOperator Plus prettyOrExpression

prettyOrExpression :: Pretty loc => Expr loc -> Doc AnsiStyle
prettyOrExpression = prettyOperator Or prettyAndExpression

prettyAndExpression :: Pretty loc => Expr loc -> Doc AnsiStyle
prettyAndExpression = prettyOperator And prettyApplicationExpression

prettyApplicationExpression :: Pretty loc => Expr loc -> Doc AnsiStyle
prettyApplicationExpression expression
  | isApplication expression = Pretty.group (Pretty.flatAlt long short)
  | otherwise = prettyPrimitiveExpression expression
  where
    isApplication Application {} = True
    isApplication _ = False

    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort Application {..} =
      prettyShort function
        <> " "
        <> prettyPrimitiveExpression argument
    prettyShort other =
      prettyPrimitiveExpression other

    prettyLong Application {..} =
      prettyLong function
        <> Pretty.hardline
        <> "  "
        <> prettyPrimitiveExpression argument
    prettyLong other =
      prettyPrimitiveExpression other

prettyPrimitiveExpression :: Pretty loc => Expr loc -> Doc AnsiStyle
prettyPrimitiveExpression Variable {..} = label (pretty name)
prettyPrimitiveExpression Builtin {..} =
  pretty builtin
prettyPrimitiveExpression Scalar {..} =
  pretty scalar
prettyPrimitiveExpression other = Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "(" <> prettyExpression other <> punctuation ")"

    long =
      Pretty.align
        ( punctuation "("
            <> " "
            <> prettyExpression other
            <> Pretty.hardline
            <> punctuation ")"
        )
