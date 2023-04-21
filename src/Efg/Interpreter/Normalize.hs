{-# LANGUAGE NamedFieldPuns #-}

module Efg.Interpreter.Normalize where

import Efg.Interpreter.Value (Env (..), Value (..))
import Efg.Parser.Location (Offset)
import Efg.Syntax (Builtin, Expr, Operator, Scalar (..))

import Data.Scientific
import qualified Efg.Interpreter.Value as Value
import Efg.Parser.Lexing (parse)
import qualified Efg.Parser.Parsing as P
import qualified Efg.Syntax as Syntax
import GHC.IO (unsafePerformIO)

lookupVariable :: Text -> Int -> Env -> IORef Value
lookupVariable name index (Env env) =
  case env of
    (key, value) : rest ->
      if key == name
        then if index == 0 then value else lookupVariable name (index - 1) (Env rest)
        else lookupVariable name index (Env rest)
    [] -> error ("Variable " <> show name <> " not found")

asInteger :: Scalar -> Int
asInteger (Natural n) = fromIntegral n
asInteger (Integer n) = fromInteger n
asInteger _ = error "Unreachable"

interp :: Env -> Expr Offset -> IO Value
interp env@(Env e) = \case
  Syntax.Variable {..} -> do
    let x = lookupVariable name 0 env
    v <- readIORef x
    case v of
      VClosure env' e' -> do
        v' <- interp env' e'
        writeIORef x v
        return v'
      v' -> pure v'
  Syntax.Scalar {..} ->
    case scalar of
      Natural n -> pure (VInt (fromIntegral n))
      Integer n -> pure (VInt (fromInteger n))
      Bool b -> pure (VBool b)
      _ -> error "Not implemented"
  Syntax.Operator {..} -> do
    l' <- interp env left
    r' <- interp env right
    case (l', r') of
      (VInt l, VInt r) ->
        case operator of
          Syntax.Plus -> pure $ Value.VInt (l + r)
          Syntax.Less -> pure $ Value.VBool (l < r)
          _ -> error "Not implemented"
      _ -> error "Not implemented"
  Syntax.Lambda {..} -> pure $ VClosure env body
  Syntax.Application {..} -> do
    c <- interp env function
    case c of
      VClosure (Env env') (Syntax.Lambda {name, body}) -> do
        n <- newIORef $ VClosure env argument
        interp (Env ((name, n) : env')) body
      _ -> error "Function expected in application"
  Syntax.If {..} -> do
    predicate' <- interp env predicate
    case predicate' of
      VBool True -> interp env ifTrue
      VBool False -> interp env ifFalse
      _ -> error "Boolean expected in if"
  Syntax.Rec {..} -> do
    let env' = Env ((name, unsafePerformIO $ newIORef $ VClosure env' expr) : e)
    interp env' expr
  _ -> error "Not implemented"

t :: Text
t = "(rec fib -> \\n -> if n < 2 then 1 else fib (n - 1) + fib (n - 2))"

testing = do
  clos <- interp (Env []) (parse "" t P.expr_)
  let env = Env [("fib", unsafePerformIO $ newIORef clos), ("n", unsafePerformIO $ newIORef $ VInt 2)]
  interp env (Syntax.Application {function = Syntax.Variable {location = 0, name = "fib", index = 0}, argument = Syntax.Scalar {location = 0, scalar = Integer 2}, location = 0})

-- >>> testing
-- Variable "n" not found

-- >>> parse "" "(rec fib -> \\n -> if n < 2 then 1 else fib (n - 1) + fib (n - 2)) 2" P.expr_
-- Application {location = 66, function = Rec {location = 1, name = "fib", expr = Lambda {location = 12, nameLocation = 13, name = "n", body = If {location = 18, predicate = Operator {location = 23, left = Variable {location = 21, name = "n", index = 0}, operatorLocation = 23, operator = Less, right = Scalar {location = 25, scalar = Integer 2}}, ifTrue = Scalar {location = 32, scalar = Integer 1}, ifFalse = Operator {location = 51, left = Application {location = 43, function = Variable {location = 39, name = "fib", index = 0}, argument = Operator {location = 46, left = Variable {location = 44, name = "n", index = 0}, operatorLocation = 46, operator = Minus, right = Scalar {location = 48, scalar = Integer 1}}}, operatorLocation = 51, operator = Plus, right = Application {location = 57, function = Variable {location = 53, name = "fib", index = 0}, argument = Operator {location = 60, left = Variable {location = 58, name = "n", index = 0}, operatorLocation = 60, operator = Minus, right = Scalar {location = 62, scalar = Integer 2}}}}}}}, argument = Scalar {location = 66, scalar = Integer 2}}

-- >>> interp (Env []) (parse "" t P.expr_)
-- Function expected in application
