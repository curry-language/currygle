------------------------------------------------------------------------------
--- Data types to be used to read CurryInfo (`.cdoc`) files.
--- The data types are taken from the module `CurryInfo.CDoc` and
--- duplicated here, since all the other stuff from the CurryDoc package
--- is not needed.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version October 2025
------------------------------------------------------------------------------

module Crawler.Types
  where

import Data.Time
import FlatCurry.Types
import FlatCurry.FlexRigid

------------------------------------------------------------------------------
--- The type to represent information about a package which consists of
--- the package identifier (a string of the form `PACKAGE-VERSION`)
--- and the last upload time (represented as an integer for efficient
--- storing and comparison) of the package.
data PackageInfo = PackageInfo
  { packageName    :: String
  , packageUpload  :: Int
  }

------------------------------------------------------------------------------

-- These are the types defined in the module `CurryDoc.CDoc`.
-- Since Currygle does not import this (large) package,
-- we just redefine the data types here.

--- The information about a Curry module contains
--- * the module information
--- * information about the function defined in the module
--- * information about types (also newtypes and classes) defined in the module
data CurryInfo = CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]
  deriving (Show,Read)

--- The base information about a module contains
--- * the name
--- * the author
--- * the description
data ModuleInfo = ModuleInfo String String String
  deriving (Show,Read)

--- The information about functions defined in a Curry module contains
--- * the name
--- * the signature
--- * the corresponding module
--- * the description
--- * `True` if the function is non-deterministically defined
--- * the flex/rigid status of the function
data FunctionInfo =
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
data TypeInfo =
  TypeInfo String [(QName, [TypeExpr])] [TVarIndex] String String Bool
  deriving (Show,Read)