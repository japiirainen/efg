import Lean.Data.Parsec

open Lean Parsec

namespace Efg

inductive Op where
  | and   : Op
  | or    : Op
  | plus  : Op
  | times : Op
  | mod   : Op
  deriving Repr

instance : ToString Op where
  toString op := match op with
    | Op.and   => "∧"
    | Op.or    => "∨"
    | Op.plus  => "+"
    | Op.times => "*"
    | Op.mod   => "%"

inductive Scalar where
  | bool : Bool   → Scalar
  | int  : Int    → Scalar
  | nat  : Nat    → Scalar
  | str  : String → Scalar
  | null : Scalar
  deriving Repr

inductive Syntax (α : Type u) where
  | operator : (location : α) → (opLocation : α) → (op : Op) → (left : Syntax α) → (right : Syntax α) → Syntax α
  | lambda   : (location : α) → (nameLocation : α) → (name : String) → (body : Syntax α) → Syntax α
  | app      : (location : α) → (function : Syntax α) → (argument : Syntax α) → Syntax α
  | var      : (location : α) →  (name : String) → (index : Nat) → Syntax α
  | scalar   : Scalar → Syntax α 
  deriving Repr

end Efg