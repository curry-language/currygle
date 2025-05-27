----------------------------------------------------------------------
--- Data types to be used to read older CurryInfo (`.cdoc`) files which
--- have been generated before base version 3.0.0.
--- Since these files contain type expressions with a different
--- representation of `ForallTypes` (see the definition of `TypeExpr`
--- in this module below), these data types are used for reading
--- if reading w.r.t. `Crawler.Types` fails.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module Crawler.AltTypes
  where

import FlatCurry.FlexRigid

-- These are the types from the module `CurryDoc.CDoc`.
-- Since Currygle does not need the operations of that module,
-- we just redefined the data types here.

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

-- This is the representation of type expressions used in FlatCurry
-- in versions before 3.0.0 where `ForallType` does not contain
-- a kind component. Since there exist documentation files (`.cdoc`)
-- with this old representation, we read them according to this
-- definition and translate it into the newer representation.
data TypeExpr = TVar Int
              | FuncType TypeExpr TypeExpr
              | TCons QName [TypeExpr]
              | ForallType [TVarIndex] TypeExpr
  deriving (Show, Read)