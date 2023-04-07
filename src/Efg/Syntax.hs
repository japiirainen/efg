{-# LANGUAGE DeriveFunctor #-}

module Efg.Syntax where

import Efg.Errors (SrcPosCtx)

-- modules

data ModuleSrcName = Prelude | User Text
  deriving stock (Show, Eq, Ord)

data UModule = UModule
  { uModuleName :: ModuleSrcName
  , uModuleImports :: [ModuleSrcName]
  , uModuleSrcBlocks :: [SrcBlock]
  }
  deriving stock (Show)

data SrcBlock = SrcBlock
  { sbLine :: Int
  , sbOffset :: Int
  , sbText :: Text
  , sbContents :: SrcBlock'
  }
  deriving stock (Show)

data SrcBlock'
  = TopDecl CTopDecl
  | UnParsable Bool String
  deriving stock (Show)

-- concrete syntax

data WithSrc a = WithSrc SrcPosCtx a
  deriving stock (Show, Functor)

type Group = WithSrc Group'

data Group'
  = CNat Word64
  | CInt Int
  | CFloat Double
  | CBin Bin Group Group
  | CIdentifier Text
  | CChar Char
  | CParens [Group]
  | CBrackets [Group]
  | CBraces [Group]
  deriving stock (Show)

type Bin = WithSrc Bin'

data Bin'
  = EvalBinOp Text
  | Dot
  deriving stock (Show, Eq, Ord)

readBin :: Text -> Bin'
readBin = \case
  "." -> Dot
  name -> EvalBinOp name

type CTopDecl = WithSrc CTopDecl'

data CTopDecl'
  = CDecl CDecl'
  deriving stock (Show)

type CDecl = WithSrc CDecl'

data CDecl'
  = CExpr Group
  deriving stock (Show)
