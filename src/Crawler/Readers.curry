----------------------------------------------------------------------
--- Data types to be used to read in CurryInfo. The Datatypes are from
--- the CurryInfo.CDoc module.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module Crawler.Readers
  where

import FlatCurry.Types
import FlatCurry.FlexRigid

-- These are the classes from the CurryDoc.CDoc module. This search engine doesn't need
-- the functions, so the pure structure of the data types is enough.

-- the name
-- the author
-- the description
data ModuleInfo = ModuleInfo String String String
  deriving (Show,Read)

-- the module
-- the corresponding functions
-- the corresponding data and type declaration
data CurryInfo = CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]
  deriving (Show,Read)

-- the name
-- the signature
-- the corresponding module
-- the description
-- True if property non-deterministically defined
-- the flex/rigid status
data FunctionInfo =
  FunctionInfo String TypeExpr String String Bool FlexRigidResult
  deriving (Show,Read)

-- the name, which has _Dict# as a prefix if it is a class
-- the constructors, as a constructor name, and a list of types
-- a list of type variables (i.e., non-empty for a polymorphic type)
-- the corresponding module
-- the description
-- a flag which is `True` if it is a type synonym
data TypeInfo =
  TypeInfo String [(QName, [TypeExpr])] [TVarIndex] String String Bool
  deriving (Show,Read)