----------------------------------------------------------------------
--- This modules defines the various items used in the index structures
--- and operations to extract information from these items.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------
{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module Index.Item
  ( IndexItem(..)
  , descriptionOfItem, moduleOfItem, packageOfItem, functionNamesOfItem
  , typeNameOfItem, classNameOfItem, authorOfItem, signatruesOfItem
  , toIndexItem )
 where

--import System.IO.Unsafe ( unsafePerformIO )
import Data.List      ( isPrefixOf )
import System.IO

import FlatCurry.Types
import FlatCurry.FlexRigid
import System.FilePath

import Crawler.Readers
import Index.Signature
import Index.Helper    ( toLowerS )
import Settings

import RW.Base

-- An `IndexItem` contains information about all entities showable by Currygle.
-- Thus, results in the various search structures always points to some
-- `IndexItem` which is either a `ModuleIItem`, a `FunctionItem`,
-- or a `TypeItem`.
-- A `ModuleItem` contains:
-- * name
-- * author(s)
-- * package
-- * description
-- A `FunctionItem` contains:
-- * name
-- * module
-- * package
-- * signature
-- * if it is deterministic
-- * if it is flexible
-- * description
-- A `TypeItem` contains:
-- * name of the type
-- * module
-- * package
-- * if it is a class
-- * constructors (names and signatures)
-- * description
data IndexItem =
    ModuleItem   String String String String
  | FunctionItem String String String [Signature] Bool FlexRigidResult String
  | TypeItem     String String String Bool [(String,[Signature])] String
  deriving (Show, Read)

-- Turns a CurryInfo data term into IndexItems which are used for the
-- search functionality of Currygle.
toIndexItem :: (CurryInfo, String) -> [IndexItem]
toIndexItem (CurryInfo modInfo funcInfos typeInfos, packName) = 
  moduleInfoToIndexItem modInfo packName :
  map (\x -> functionInfoToIndexItem x packName) funcInfos ++
  map (\x -> typeInfoToIndexItem x packName) typeInfos

-- Turns a ModuleInfo into a ModuleIndex, used as an index for the search.
-- It needs the package name as extra input.
moduleInfoToIndexItem :: ModuleInfo -> String -> IndexItem
moduleInfoToIndexItem (ModuleInfo name author des) packageName = 
  ModuleItem name author packageName des

-- Turns a FunctionInfo into a FunctionIndex, used as an index for the search.
-- It needs the package name as extra input.
functionInfoToIndexItem :: FunctionInfo -> String -> IndexItem
functionInfoToIndexItem
  (FunctionInfo name signature modName des nonDet flexRigid) packageName =
  FunctionItem name modName packageName
    (seperateSig (typeExprToSignature signature)) (not nonDet) flexRigid des

-- Turns a TypeInfo into a TypeIndex, used as an index for the search.
-- It needs the package name as extra input.
typeInfoToIndexItem :: TypeInfo -> String -> IndexItem
typeInfoToIndexItem (TypeInfo name constructors vars modName des _) package =
  TypeItem (stripClassNamePrefix name) modName package (isClassName name)
    (constrToSigs constructors (stripClassNamePrefix name) vars) des

-- Takes a list of Constructors, the name of the type, and the variables it has,
-- and creates a list of lists of constructor signatures,
-- where each list of signature represents a constructor
constrToSigs :: [(QName, [TypeExpr])] -> String -> [Int]
             -> [(String,[Signature])]
constrToSigs constr name vars  =
  map (\(c,sig) -> (c, sig ++ [getType name vars])) (map constrToSig constr)
 where
  constrToSig :: (QName, [TypeExpr]) -> (String,[Signature])
  constrToSig ((_,c), exprs) = (c, map typeExprToSignature exprs)

  getType :: String -> [Int] -> Signature
  getType name1 vars1 =
    Index.Signature.Type name1 (map Index.Signature.Var vars1)


-- Gets a type exprerssion, and turns it into a list of type expressions,
-- where each entry is one argument of the original Function.
-- example: Int -> Int becomes [Int, Int]
typeExprToTypeExprList :: TypeExpr -> [TypeExpr]
typeExprToTypeExprList (FuncType f1 f2) = f1 : (typeExprToTypeExprList f2)
typeExprToTypeExprList (TVar x) = [TVar x]
typeExprToTypeExprList (TCons n ex) = [TCons n ex]
typeExprToTypeExprList (ForallType a b) = [ForallType a b]

combineTypeExpr :: [TypeExpr] -> TypeExpr
combineTypeExpr []     = TVar 5 -- should never happen, avoid warning
combineTypeExpr (x:xs) | length xs == 0 = x
                       | otherwise      = FuncType x (combineTypeExpr xs)

--------------
-- Get methods
--------------

-- For an IndexItem, returns the words in the description of the item.
descriptionOfItem :: IndexItem -> [String]
descriptionOfItem (ModuleItem   _ _ _       d) = wordsOfDescription d
descriptionOfItem (FunctionItem _ _ _ _ _ _ d) = wordsOfDescription d
descriptionOfItem (TypeItem     _ _ _ _ _   d) = wordsOfDescription d

-- Extracts the relevant words of a description. These are the words a
-- leading and trailing non-letters are deleted and the remaining consists
-- of letters only.
wordsOfDescription :: String -> [String]
wordsOfDescription =
  filter realWord . map toLowerS . map stripNonLetters . words
--wordsOfDescription s =
  --let ss = filter realWord . map toLowerS . map stripNonLetters . words $ s
  --in unsafePerformIO (appendFile "WORDS" (unlines ss) >> return ss)
 where
  realWord w = all isAlpha w && length w > 2 && w `notElem` excludedWords

  stripNonLetters =
    reverse . dropWhile (not . isAlpha) . reverse . dropWhile (not . isAlpha)

-- List of words (with more than two characters) excluded from description
-- index (might be extended or go into a file).
excludedWords :: [String]
excludedWords =
  [ "all", "and", "any", "are","can","does", "for", "has", "have"
  , "its", "made", "make", "must", "not", "only", "that", "the", "them"
  , "then", "they", "than", "this", "use", "used", "will", "with" ]

-- For an IndexItem, returns the name of the module (if present).
moduleOfItem :: IndexItem -> [String]
moduleOfItem (ModuleItem mname _ _ _)         = [toLowerS mname]
moduleOfItem (FunctionItem _ mname _ _ _ _ _) = [toLowerS mname]
moduleOfItem (TypeItem     _ mname _ _ _ _)   = [toLowerS mname]

-- Gets an IndexItem, and reads out the name of the package
packageOfItem :: IndexItem -> [String]
packageOfItem (ModuleItem   _ _ pname _)       = [toLowerS pname]
packageOfItem (FunctionItem _ _ pname _ _ _ _) = [toLowerS pname]
packageOfItem (TypeItem     _ _ pname _ _ _)   = [toLowerS pname]

-- Returns the names of functions and constructors occurring in an `IndexItem`.
functionNamesOfItem :: IndexItem -> [String]
functionNamesOfItem (ModuleItem   _ _ _ _)      = []
functionNamesOfItem (FunctionItem funcName _ _ _ _ _ _) = [toLowerS funcName]
functionNamesOfItem (TypeItem     _ _ _ _ cs _) = map (toLowerS . fst) cs

typeNameOfItem :: IndexItem -> [String]
typeNameOfItem (ModuleItem   _ _ _ _)             = []
typeNameOfItem (FunctionItem _ _ _ _ _ _ _)       = []
typeNameOfItem (TypeItem     tname _ _ False _ _) = [toLowerS tname]
typeNameOfItem (TypeItem     _ _ _ True _ _)      = []

classNameOfItem :: IndexItem -> [String]
classNameOfItem (ModuleItem _ _ _ _)          = []
classNameOfItem (FunctionItem _ _ _ _ _ _ _)  = []
classNameOfItem (TypeItem _ _ _ False _ _)    = []
classNameOfItem (TypeItem cname _ _ True _ _) = [toLowerS cname]

-- Gets an IndexItem, and reads out the name of the author
authorOfItem :: IndexItem -> [String]
authorOfItem (ModuleItem _ author _ _)    = [toLowerS author]
authorOfItem (FunctionItem _ _ _ _ _ _ _) = []
authorOfItem (TypeItem     _ _ _ _ _ _)   = []

signatruesOfItem :: IndexItem -> Maybe [[Signature]]
signatruesOfItem (ModuleItem   _ _ _ _)          = Nothing
signatruesOfItem (FunctionItem _ _ _ sig _ _ _)  = Just [sig]
signatruesOfItem (TypeItem     _ _ _ _ constr _) = Just (map snd constr)

------------------------------------------------------------------------------
-- Auto generated by curry-rw-data
------------------------------------------------------------------------------

instance ReadWrite FlexRigidResult where
  readRW _ ('0' : r0) = (UnknownFR,r0)
  readRW _ ('1' : r0) = (ConflictFR,r0)
  readRW _ ('2' : r0) = (KnownFlex,r0)
  readRW _ ('3' : r0) = (KnownRigid,r0)

  showRW _ strs0 UnknownFR = (strs0,showChar '0')
  showRW _ strs0 ConflictFR = (strs0,showChar '1')
  showRW _ strs0 KnownFlex = (strs0,showChar '2')
  showRW _ strs0 KnownRigid = (strs0,showChar '3')

  writeRW _ h UnknownFR strs = hPutChar h '0' >> return strs
  writeRW _ h ConflictFR strs = hPutChar h '1' >> return strs
  writeRW _ h KnownFlex strs = hPutChar h '2' >> return strs
  writeRW _ h KnownRigid strs = hPutChar h '3' >> return strs

  typeOf _ = monoRWType "FlexRigidResult"

instance ReadWrite IndexItem where
  readRW strs ('0' : r0) = (ModuleItem a' b' c' d',r4)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1
      (c',r3) = readRW strs r2
      (d',r4) = readRW strs r3
  readRW strs ('1' : r0) = (FunctionItem a' b' c' d' e' f' g',r7)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1
      (c',r3) = readRW strs r2
      (d',r4) = readRW strs r3
      (e',r5) = readRW strs r4
      (f',r6) = readRW strs r5
      (g',r7) = readRW strs r6
  readRW strs ('2' : r0) = (TypeItem a' b' c' d' e' f',r6)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1
      (c',r3) = readRW strs r2
      (d',r4) = readRW strs r3
      (e',r5) = readRW strs r4
      (f',r6) = readRW strs r5

  showRW params strs0 (ModuleItem a' b' c' d') =
    (strs4,showChar '0' . (show1 . (show2 . (show3 . show4))))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'
      (strs3,show3) = showRW params strs2 c'
      (strs4,show4) = showRW params strs3 d'
  showRW params strs0 (FunctionItem a' b' c' d' e' f' g') =
    (strs7
    ,showChar '1'
      . (show1 . (show2 . (show3 . (show4 . (show5 . (show6 . show7)))))))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'
      (strs3,show3) = showRW params strs2 c'
      (strs4,show4) = showRW params strs3 d'
      (strs5,show5) = showRW params strs4 e'
      (strs6,show6) = showRW params strs5 f'
      (strs7,show7) = showRW params strs6 g'
  showRW params strs0 (TypeItem a' b' c' d' e' f') =
    (strs6
    ,showChar '2' . (show1 . (show2 . (show3 . (show4 . (show5 . show6))))))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'
      (strs3,show3) = showRW params strs2 c'
      (strs4,show4) = showRW params strs3 d'
      (strs5,show5) = showRW params strs4 e'
      (strs6,show6) = showRW params strs5 f'

  writeRW params h (ModuleItem a' b' c' d') strs =
    hPutChar h '0'
     >> (((writeRW params h a' strs >>= writeRW params h b')
           >>= writeRW params h c')
          >>= writeRW params h d')
  writeRW params h (FunctionItem a' b' c' d' e' f' g') strs =
    hPutChar h '1'
     >> ((((((writeRW params h a' strs >>= writeRW params h b')
              >>= writeRW params h c')
             >>= writeRW params h d')
            >>= writeRW params h e')
           >>= writeRW params h f')
          >>= writeRW params h g')
  writeRW params h (TypeItem a' b' c' d' e' f') strs =
    hPutChar h '2'
     >> (((((writeRW params h a' strs >>= writeRW params h b')
             >>= writeRW params h c')
            >>= writeRW params h d')
           >>= writeRW params h e')
          >>= writeRW params h f')

  typeOf _ = monoRWType "IndexItem"

------------------------------------------------------------------------------
