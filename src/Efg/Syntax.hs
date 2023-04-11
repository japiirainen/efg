{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}

module Efg.Syntax (
  ) where

import Control.Lens (Plated (..))
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Scientific (Scientific)
import Data.Sequence (Seq ((:<|)))
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Efg.Pretty (Pretty (..), keyword, label, punctuation)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Natural (Natural)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Data.Text as Text
import qualified Efg.Pretty as Pretty
import qualified Prettyprinter as Pretty

type Name = Text

data TopDecl s a
  = Expr {expr :: Expr s a}
  | Def {defName :: Text, expr :: Expr s a}

data Expr s a
  = Variable {location :: s, name :: Text}
  | Operator {location :: s, left :: Expr s a, operatorLocation :: s, operator :: Operator, right :: Expr s a}
  deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

data Operator
  = And
  | Or
  | Plus
  | Times
  deriving stock (Eq, Generic, Lift, Show)