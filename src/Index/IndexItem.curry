{-# OPTIONS_FRONTEND -Wno-incomplete-patterns -Wno-unused-bindings #-}

module Index.IndexItem
    where

import Index.Signature

import FlatCurry.Types
import FlatCurry.FlexRigid
import Data.List
import Data.Char
import System.CurryPath
import System.FilePath

import Crawler.Readers
import Settings

import RW.Base
import System.IO

-- Either a ModuleIndex, a FunctionIndex or a TypeIndex
data IndexItem = ModuleItem ModuleIndex
               | FunctionItem FunctionIndex
               | TypeItem TypeIndex
  deriving (Show, Read)

-- The module index contains:
-- The name
-- The author(s)
-- The package
-- The Url of the online Docs
-- The description
data ModuleIndex = ModuleIndex String String String String String
  deriving (Show, Read)

-- The name
-- The module
-- The package
-- The signature
-- If it is deterministic
-- If it is flexible
-- The Url of the online Docs
-- The description
data FunctionIndex = FunctionIndex String String String [Signature] Bool FlexRigidResult String String
  deriving (Show, Read)

-- The name of the type
-- The module
-- The package
-- If it is a class
-- The constructors
-- The Url of the online Docs
-- The description
data TypeIndex = TypeIndex String String String Bool [[Signature]] String String
  deriving (Show, Read)

-- Gets a CurryInfo, and turns it into IndexItems, which are used for the search functionality of currygle2
toIndexItem :: (CurryInfo, String) -> [IndexItem]
toIndexItem (CurryInfo modInfo funcInfos typeInfos, packName) = 
                                                    (moduleInfoToIndexItem modInfo packName
                                                    :(map (\x -> functionInfoToIndexItem x packName) funcInfos))
                                                    ++ (map (\x -> typeInfoToIndexItem x packName) typeInfos)

-- Turns a ModuleInfo into a ModuleIndex, used as an index for the search. It needs the package name as extra input.
moduleInfoToIndexItem :: ModuleInfo -> String -> IndexItem
moduleInfoToIndexItem (ModuleInfo name author des) packageName = 
    ModuleItem (
      ModuleIndex
        (name)
        (author)
        (packageName)
        (moduleDocumentationUrl packageName name)
        des)

-- Turns a FunctionInfo into a FunctionIndex, used as an index for the search. It needs the package name as extra input.
functionInfoToIndexItem :: FunctionInfo -> String -> IndexItem
functionInfoToIndexItem
  (FunctionInfo name signature modName des nonDet flexRigid) packageName =
    FunctionItem (
      FunctionIndex
        (name)
        (modName)
        (packageName)
        (seperateSig (typeExprToSignature signature))
        (not nonDet)
        flexRigid
        (entityDocumentationUrl packageName modName name)
        des)

-- Turns a TypeInfo into a TypeIndex, used as an index for the search. It needs the package name as extra input.
typeInfoToIndexItem :: TypeInfo -> String -> IndexItem
typeInfoToIndexItem (TypeInfo name constructors vars modName des _) package =
    TypeItem (
      TypeIndex
        ((getName name))
        (modName)
        (package)
        (isClass name)
        (constrToSigs constructors (getName name) vars)
        (entityDocumentationUrl package modName name)
        des)

-- Gets a name from a TypeInfo, and returns the name for the TypeIndex.
-- If the TypeInfo was a class, it has a prefix to indicate that,
-- which this function gets rid of
getName :: String -> String
getName name = if isClass name then drop 6 name else name

isClass :: String -> Bool
isClass = isPrefixOf "_Dict#"

-- Gets the name of a Module, and returns the name of the package
findPackageName :: String -> IO String
findPackageName modName =  do
  paths <- lookupModuleSourceInLoadPath modName
  return $ case paths of Just (dirPath, _) -> getPackageName dirPath
                         Nothing           -> ""
  where
    getPackageName :: String -> String
    getPackageName path = let directories = (splitPath path) in
                            -- Take the second to last element of the path, and delete the / at the end of it
                            take (length (directories !! (length directories -2)) -1) (directories !! (length directories -2))

-- Takes a list of Constructors, the name of the type, and the variables it has,
-- and creates a list of lists of signatures, where each list of signature represents a constructor
constrToSigs :: [(QName, [TypeExpr])] -> String -> [Int] -> [[Signature]]
constrToSigs constr name vars  = map (\c -> c ++ [(getType name vars)]) (map constrToSig constr)
  where
    constrToSig :: (QName, [TypeExpr]) -> [Signature]
    constrToSig (_, exprs) = map typeExprToSignature exprs
    getType :: String -> [Int] -> Signature
    getType name1 vars1 = Index.Signature.Type name1 (map Index.Signature.Var vars1)


-- Gets a type exprerssion, and turns it into a list of type expressions,
-- where each entry is one argument of the original Function.
-- example: Int -> Int becomes [Int, Int]
typeExprToTypeExprList :: TypeExpr -> [TypeExpr]
typeExprToTypeExprList (FuncType f1 f2) = f1 : (typeExprToTypeExprList f2)
typeExprToTypeExprList (TVar x) = [TVar x]
typeExprToTypeExprList (TCons n ex) = [TCons n ex]
typeExprToTypeExprList (ForallType a b) = [ForallType a b]

combineTypeExpr :: [TypeExpr] -> TypeExpr
-- Case should never happen, but the warning is annoying
combineTypeExpr []     = TVar 5
combineTypeExpr (x:xs) | length xs == 0 = x
                       | otherwise      = FuncType x (combineTypeExpr xs)

--------------
-- Get methods
--------------

-- Gets an IndexItem, and reads out the name of the module
getModule :: IndexItem -> Maybe String
getModule (ModuleItem (ModuleIndex modName _ _ _ _)) = Just  $ toLowerStr modName
getModule (FunctionItem (FunctionIndex _ modName _ _ _ _ _ _)) = Just  $ toLowerStr modName
getModule (TypeItem (TypeIndex _ modName _ _ _ _ _)) = Just  $ toLowerStr modName

-- Gets an IndexItem, and reads out the name of the package
getPackage :: IndexItem -> Maybe String
getPackage (ModuleItem (ModuleIndex _ _ pakName _ _)) = Just  $ toLowerStr pakName
getPackage (FunctionItem (FunctionIndex _ _ pakName _ _ _ _ _)) = Just  $ toLowerStr pakName
getPackage (TypeItem (TypeIndex _ _ pakName _ _ _ _)) = Just  $ toLowerStr pakName

getFunctionName :: IndexItem -> Maybe String
getFunctionName (ModuleItem (ModuleIndex _ _ _ _ _)) = Nothing
getFunctionName (FunctionItem (FunctionIndex funcName _ _ _ _ _ _ _)) = Just  $ toLowerStr funcName
getFunctionName (TypeItem (TypeIndex _ _ _ _ _ _ _)) = Nothing

getTypeName :: IndexItem -> Maybe String
getTypeName (ModuleItem (ModuleIndex _ _ _ _ _)) = Nothing
getTypeName (FunctionItem (FunctionIndex _ _ _ _ _ _ _ _)) = Nothing
getTypeName (TypeItem (TypeIndex typeName _ _ False _ _ _)) = Just  $ toLowerStr typeName
getTypeName (TypeItem (TypeIndex _ _ _ True _ _ _)) = Nothing

getClassName :: IndexItem -> Maybe String
getClassName (ModuleItem (ModuleIndex _ _ _ _ _)) = Nothing
getClassName (FunctionItem (FunctionIndex _ _ _ _ _ _ _ _)) = Nothing
getClassName (TypeItem (TypeIndex _ _ _ False _ _ _)) = Nothing
getClassName (TypeItem (TypeIndex className _ _ True _ _ _)) = Just $ toLowerStr className

-- Gets an IndexItem, and reads out the name of the author
getAuthor :: IndexItem -> Maybe String
getAuthor (ModuleItem (ModuleIndex _ author _ _ _)) = Just $ toLowerStr author
getAuthor (FunctionItem (FunctionIndex _ _ _ _ _ _ _ _)) = Nothing
getAuthor (TypeItem (TypeIndex _ _ _ _ _ _ _)) = Nothing

getSignatures :: IndexItem -> Maybe [[Signature]]
getSignatures (ModuleItem (ModuleIndex _ _ _ _ _)) = Nothing
getSignatures (FunctionItem (FunctionIndex _ _ _ sig _ _ _ _)) = Just [sig]
getSignatures (TypeItem (TypeIndex _ _ _ _ constr _ _)) = Just constr

toLowerStr :: String -> String
toLowerStr s = map toLower s

----------------------------------
-- Auto generated stuff fo rw-data
----------------------------------
instance ReadWrite FlexRigidResult where
  readRW strs ('0' : r0) = (UnknownFR,r0)
  readRW strs ('1' : r0) = (ConflictFR,r0)
  readRW strs ('2' : r0) = (KnownFlex,r0)
  readRW strs ('3' : r0) = (KnownRigid,r0)

  showRW params strs0 UnknownFR = (strs0,showChar '0')
  showRW params strs0 ConflictFR = (strs0,showChar '1')
  showRW params strs0 KnownFlex = (strs0,showChar '2')
  showRW params strs0 KnownRigid = (strs0,showChar '3')

  writeRW params h UnknownFR strs = hPutChar h '0' >> return strs
  writeRW params h ConflictFR strs = hPutChar h '1' >> return strs
  writeRW params h KnownFlex strs = hPutChar h '2' >> return strs
  writeRW params h KnownRigid strs = hPutChar h '3' >> return strs

  typeOf _ = monoRWType "FlexRigidResult"

instance ReadWrite IndexItem where
  readRW strs ('0' : r0) = (ModuleItem a',r1)
    where
      (a',r1) = readRW strs r0
  readRW strs ('1' : r0) = (FunctionItem a',r1)
    where
      (a',r1) = readRW strs r0
  readRW strs ('2' : r0) = (TypeItem a',r1)
    where
      (a',r1) = readRW strs r0

  showRW params strs0 (ModuleItem a') = (strs1,showChar '0' . show1)
    where
      (strs1,show1) = showRW params strs0 a'
  showRW params strs0 (FunctionItem a') = (strs1,showChar '1' . show1)
    where
      (strs1,show1) = showRW params strs0 a'
  showRW params strs0 (TypeItem a') = (strs1,showChar '2' . show1)
    where
      (strs1,show1) = showRW params strs0 a'

  writeRW params h (ModuleItem a') strs =
    hPutChar h '0' >> writeRW params h a' strs
  writeRW params h (FunctionItem a') strs =
    hPutChar h '1' >> writeRW params h a' strs
  writeRW params h (TypeItem a') strs =
    hPutChar h '2' >> writeRW params h a' strs

  typeOf _ = monoRWType "IndexItem"

instance ReadWrite ModuleIndex where
  readRW strs r0 = (ModuleIndex a' b' c' d' e',r5)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1
      (c',r3) = readRW strs r2
      (d',r4) = readRW strs r3
      (e',r5) = readRW strs r4

  showRW params strs0 (ModuleIndex a' b' c' d' e') =
    (strs5,show1 . (show2 . (show3 . (show4 . show5))))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'
      (strs3,show3) = showRW params strs2 c'
      (strs4,show4) = showRW params strs3 d'
      (strs5,show5) = showRW params strs4 e'

  writeRW params h (ModuleIndex a' b' c' d' e') strs =
    (((writeRW params h a' strs >>= writeRW params h b')
       >>= writeRW params h c')
      >>= writeRW params h d')
     >>= writeRW params h e'

  typeOf _ = monoRWType "ModuleIndex"

instance ReadWrite FunctionIndex where
  readRW strs r0 = (FunctionIndex a' b' c' d' e' f' g' h',r8)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1
      (c',r3) = readRW strs r2
      (d',r4) = readRW strs r3
      (e',r5) = readRW strs r4
      (f',r6) = readRW strs r5
      (g',r7) = readRW strs r6
      (h',r8) = readRW strs r7

  showRW params strs0 (FunctionIndex a' b' c' d' e' f' g' h') =
    (strs8
    ,show1
      . (show2 . (show3 . (show4 . (show5 . (show6 . (show7 . show8)))))))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'
      (strs3,show3) = showRW params strs2 c'
      (strs4,show4) = showRW params strs3 d'
      (strs5,show5) = showRW params strs4 e'
      (strs6,show6) = showRW params strs5 f'
      (strs7,show7) = showRW params strs6 g'
      (strs8,show8) = showRW params strs7 h'

  writeRW params h (FunctionIndex a' b' c' d' e' f' g' h') strs =
    ((((((writeRW params h a' strs >>= writeRW params h b')
          >>= writeRW params h c')
         >>= writeRW params h d')
        >>= writeRW params h e')
       >>= writeRW params h f')
      >>= writeRW params h g')
     >>= writeRW params h h'

  typeOf _ = monoRWType "FunctionIndex"

instance ReadWrite TypeIndex where
  readRW strs r0 = (TypeIndex a' b' c' d' e' f' g',r7)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1
      (c',r3) = readRW strs r2
      (d',r4) = readRW strs r3
      (e',r5) = readRW strs r4
      (f',r6) = readRW strs r5
      (g',r7) = readRW strs r6

  showRW params strs0 (TypeIndex a' b' c' d' e' f' g') =
    (strs7,show1 . (show2 . (show3 . (show4 . (show5 . (show6 . show7))))))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'
      (strs3,show3) = showRW params strs2 c'
      (strs4,show4) = showRW params strs3 d'
      (strs5,show5) = showRW params strs4 e'
      (strs6,show6) = showRW params strs5 f'
      (strs7,show7) = showRW params strs6 g'

  writeRW params h (TypeIndex a' b' c' d' e' f' g') strs =
    (((((writeRW params h a' strs >>= writeRW params h b')
         >>= writeRW params h c')
        >>= writeRW params h d')
       >>= writeRW params h e')
      >>= writeRW params h f')
     >>= writeRW params h g'

  typeOf _ = monoRWType "TypeIndex"
