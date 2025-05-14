----------------------------------------------------------------------
--- Data types to be used to read in CurryInfo. It is an alternative way
--- to parse, in case the other reader fails.
---
--- @author Helge Knof
--- @version 07.12.2024
----------------------------------------------------------------------

module Crawler.ReadersAlternatives
  where

import FlatCurry.FlexRigid

-- These are the classes from the CurryDoc.CDoc module. This search engine doesn't need
-- the functions, so the pure structure of the data types is enough.

type TVarIndex = Int

type QName = ([Char], [Char])

-- the module
-- the corresponding functions
-- the corresponding data and type declaration
data AltCurryInfo = CurryInfo AltModuleInfo [AltFunctionInfo] [AltTypeInfo]
  deriving (Show,Read)

-- the name
-- the author
-- the description
data AltModuleInfo = ModuleInfo String String String
  deriving (Show,Read)

-- the name
-- the signature
-- the corresponding module
-- the description
-- True if property ist defined non-deterministically
-- the flex/rigid status
data AltFunctionInfo =
  FunctionInfo String TypeExpr String String Bool FlexRigidResult
  deriving (Show,Read)

-- the name, which has _Dict# as a prefix if it is a class
-- the constructors, as a constructor name, and a list of types
-- TVarIndex is a number, no idea what it does
-- the corresponding module
-- the description
-- A unknown bool
data AltTypeInfo =
  TypeInfo String [(QName, [TypeExpr])] [TVarIndex] String String Bool
  deriving (Show,Read)

data TypeExpr = TVar Int
              | FuncType TypeExpr TypeExpr
              | TCons QName [TypeExpr]
              | ForallType [TVarIndex] TypeExpr
  deriving (Show, Read)