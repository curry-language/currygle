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

-- These are the types defined in the module `CurryDoc.CDoc`.
-- Since Currygle does not import this (large) packages,
-- we just redefine the data types here.

type TVarIndex = Int

type QName = ([Char], [Char])

--- The information about a Curry module contains
--- * the module information
--- * information about the function defined in the module
--- * information about types (also newtypes and classes) defined in the module
data AltCurryInfo = CurryInfo AltModuleInfo [AltFunctionInfo] [AltTypeInfo]
  deriving (Show,Read)

--- The base information about a module contains
--- * the name
--- * the author
--- * the description
data AltModuleInfo = ModuleInfo String String String
  deriving (Show,Read)

--- The information about functions defined in a Curry module contains
--- * the name
--- * the signature
--- * the corresponding module
--- * the description
--- * `True` if the function is non-deterministically defined
--- * the flex/rigid status of the function
data AltFunctionInfo =
  FunctionInfo String TypeExpr String String Bool FlexRigidResult
  deriving (Show,Read)

--- The information about types defined in a Curry module contains
--- * the name (which has `_Dict#` as a prefix if it is a class)
--- * a list of constructor names and their argument types (or the type name
---   and the type expression in case of type synonyms)
--- * a list of type variables (which is non-empty for a polymorphic type)
--- * the corresponding module
--- * the description
--- * a flag which is `True` if it is a type synonym
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
