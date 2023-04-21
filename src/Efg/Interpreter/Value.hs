module Efg.Interpreter.Value (
  Value (..),
  Env (..),
)
where

import Efg.Parser.Location (Offset)
import Efg.Syntax (Builtin, Expr, Operator, Scalar)
import GHC.Show (Show (..))
import Prelude hiding (show)

data Env = Env [(Text, IORef Value)]

data Value
  = VInt Int
  | VBool Bool
  | VClosure Env (Expr Offset)

instance Show Value where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VClosure _ _) = "<closure>"