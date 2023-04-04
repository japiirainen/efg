{-# LANGUAGE CPP #-}

module Efg.Errors (
  Err (..),
  Errs (..),
  Except (..),
  ErrType (..),
  ErrCtx (..),
  SrcPosCtx,
  SrcTextCtx,
  SrcPos,
  Fallible (..),
  Catchable (..),
  CtxReader (..),
  catchErrExcept,
  FallibleM (..),
) where

import Control.Monad (liftM, liftM2)

type SrcPos = (Int, Int)

type SrcPosCtx = Maybe SrcPos

type SrcTextCtx = Maybe (Int, Text) -- (offset, text)

data ErrCtx = ErrCtx
  { srcTextCtx :: SrcTextCtx
  , srcPosCtx :: SrcPosCtx
  , messageCtx :: [Text]
  , stackCtx :: Maybe [Text]
  }
  deriving stock (Show, Eq)

instance Semigroup ErrCtx where
  ErrCtx text pos ctxStrs stk <> ErrCtx text' pos' ctxStrs' stk' =
    ErrCtx
      (leftmostJust text text')
      (rightmostJust pos pos')
      (ctxStrs <> ctxStrs')
      (leftmostJust stk stk')

instance Monoid ErrCtx where
  mempty = ErrCtx Nothing Nothing [] Nothing

leftmostJust :: Maybe a -> Maybe a -> Maybe a
leftmostJust (Just x) _ = Just x
leftmostJust Nothing y = y

rightmostJust :: Maybe a -> Maybe a -> Maybe a
rightmostJust = flip leftmostJust

data Err = Err
  { errType :: ErrType
  , errCtx :: ErrCtx
  , errorMessage :: Text
  }
  deriving stock (Show, Eq)

newtype Errs = Errs [Err]
  deriving newtype (Show, Eq, Semigroup, Monoid)

data ErrType
  = ParseError
  | SyntaxError
  | MonadFailErr
  | CompilerErr
  deriving stock (Show, Eq)

class MonadFail m => Fallible m where
  throwErrs :: Errs -> m a
  addErrCtx :: ErrCtx -> m a -> m a

class Fallible m => Catchable m where
  catchErr :: m a -> (Errs -> m a) -> m a

catchErrExcept :: Catchable m => m a -> m (Except a)
catchErrExcept m = catchErr (Success <$> m) (pure . Failure)

class Fallible m => CtxReader m where
  getErrCtx :: m ErrCtx

class Fallible m => FallibleApplicative m where
  mergeErrs :: m a -> m b -> m (a, b)

newtype FallibleM a = FallibleM {runFallibleM :: ReaderT ErrCtx Except a}
  deriving newtype (Functor, Applicative, Monad)

instance Fallible FallibleM where
  throwErrs (Errs errs) = FallibleM $ ReaderT \ambientCtx ->
    throwErrs $ Errs $ [Err errType (ambientCtx <> ctx) msg | Err errType ctx msg <- errs]
  addErrCtx ctx (FallibleM m) = FallibleM (local (<> ctx) m)

data Except a
  = Failure Errs
  | Success a
  deriving stock (Show, Eq)

instance Functor Except where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative Except where
  pure = Success
  {-# INLINE pure #-}
  liftA2 = liftM2
  {-# INLINE liftA2 #-}

instance Monad Except where
  return = pure
  {-# INLINE return #-}
  Failure errs >>= _ = Failure errs
  Success a >>= f = f a
  {-# INLINE (>>=) #-}

instance MonadFail Except where
  fail msg = Failure (Errs [Err CompilerErr mempty (toText msg)])

throw :: Fallible m => ErrType -> Text -> m a
throw errTy s = throwErrs $ Errs [addCompilerStackCtx $ Err errTy mempty s]
{-# INLINE throw #-}

addCompilerStackCtx :: Err -> Err
addCompilerStackCtx (Err ty ctx msg) = Err ty ctx {stackCtx = compilerStack} msg
  where
#ifdef EFG_DEBUG
    compilerStack = getCurrentCallStack ()
#else
    compilerStack = stackCtx ctx
#endif

instance MonadFail FallibleM where
  fail msg = throw MonadFailErr (toText msg)
  {-# INLINE fail #-}

instance Fallible Except where
  throwErrs = Failure
  {-# INLINE throwErrs #-}
  addErrCtx _ (Success ans) = Success ans
  addErrCtx ctx (Failure (Errs errs)) =
    Failure $ Errs [Err errType (ctx <> errCtx) msg | Err errType errCtx msg <- errs]
  {-# INLINE addErrCtx #-}
