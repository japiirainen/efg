import Lean.Data.Parsec

open Lean Parsec

namespace Efg.Syntax

variable {α : Type} {β : Type}

structure Variable where
  location : α
  name     : String
  index    : Nat

structure Operator where
  location   : α
  opLocation : α
  op         : Op
  left       : Syntax
  right      : Syntax

structure Lambda where
  location     : α
  nameLocation : α
  name         : String
  body         : Syntax

structure App where
  location : α
  function : Syntax
  argument : Syntax

inductive Scalar where
  | bool : Bool → Scalar
  | int  : Int  → Scalar
  | nat  : Nat  → Scalar
  | str  : String → Scalar
  | null : Scalar

inductive Syntax where
  | variable : Variable → Syntax
  | operator : Operator → Syntax
  | lambda   : Lambda   → Syntax
  | app      : App → Syntax 
  | scalar   : Scalar → Syntax

inductive Op where
  | and   : Op
  | or    : Op
  | plus  : Op
  | times : Op
  | mod   : Op

instance : ToString Op where
  toString op := match op with
    | Op.and   => "∧"
    | Op.or    => "∨"
    | Op.plus  => "+"
    | Op.times => "*"
    | Op.mod   => "%"

end Efg.Syntax