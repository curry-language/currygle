----------------------------------------------------------------------
--- This module collects the CurryInfo from files, and returns it as a
--- list, so the indexer can use it.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module Crawler.Crawler ( getAllCurryInfo )
  where

import Data.List
import System.IO

import FlatCurry.Types
import System.Directory
import System.FilePath  ( (</>), isExtensionOf, splitDirectories )

import Crawler.Types
import Crawler.AltTypes
import Settings

-- Searches all CDoc files in the given directory and returns their names
-- with the corresponding package name (computed from the file path).
getCDocPackages :: FilePath -> IO [(String, String)]
getCDocPackages path = do
  cdocfiles <- getCDocFilePaths path
  putStrLn $ "Number of cdoc files to be processed: " ++ show (length cdocfiles)
  return $ map addPackageName cdocfiles
 where
  -- Gets for a given `.cdoc` file path a pair consisting of the file path
  -- and the package name (which is the directory containing the `.cdoc` file).
  addPackageName fn =
    let fns = splitDirectories fn
    in if length fns > 1 then (fn, last (init fns))
                         else (fn, "")

--- Returns the paths of all `.cdoc` files contained in the directory
--- (or in subdirectories) provided as an argument.
getCDocFilePaths :: FilePath -> IO [FilePath]
getCDocFilePaths path = do
  paths <- getDirectoryContents path
  let dirpaths = map (path </>) $ filter (`notElem` [".", ".."]) paths
  concat <$> mapM getCDocs dirpaths
 where
  getCDocs :: String -> IO [String]
  getCDocs path1 = if isExtensionOf "cdoc" path1
                     then return [path1]
                     else do isfile <- doesFileExist path1
                             if isfile then return []
                                       else getCDocFilePaths path1

-- Searches in the given directory path all .cdoc files and returns
-- their contents.
getAllCurryInfo :: FilePath -> IO [(CurryInfo, String)]
getAllCurryInfo path = do
  fcntpkgs <- getCDocPackages path
  cipkgs <- mapM readCDoc fcntpkgs
  return $ [ (ci,pn) | (Just ci, pn) <- cipkgs ]
 where
  readCDoc (fn, pn) = do
    putStrLn $ "Reading file '" ++ fn ++ "'..."
    cdoc <- openFile fn ReadMode >>= hGetContents
    return (readCurryInfo cdoc, pn)

-- Reads a CurryInfo from a String. Can handle the two formats,
-- which are used in existing .cdoc files.
readCurryInfo :: String -> Maybe CurryInfo
readCurryInfo text
  | null (readToCurryInfo text)
  = let infos = readToAltCurryInfo text in
    if null infos then Nothing
                  else Just (toCurryInfo (fst (infos !! 0 )))
  | otherwise
  = Just (fst (readToCurryInfo text !! 0))
 where
  -- Interpretes a String as a CurryInfo
  readToCurryInfo :: String -> [(CurryInfo, String)]
  readToCurryInfo curryInfo = reads curryInfo

  -- Interpretes a String as a AltCurryInfo
  readToAltCurryInfo :: String -> [(AltCurryInfo, String)]
  readToAltCurryInfo curryInfo = reads curryInfo

  -- Creates a CurryInfo from an AltCurryInfo by deleting all ForallType TypeExpr
  toCurryInfo :: AltCurryInfo -> CurryInfo
  toCurryInfo (Crawler.AltTypes.CurryInfo
                 altModInfo altFuncInfos altTypeInfos) =
    Crawler.Types.CurryInfo
        (toModInfo altModInfo)
        (toFuncInfos altFuncInfos)
        (map toTypeInfo altTypeInfos)
  
  toModInfo :: AltModuleInfo -> ModuleInfo
  toModInfo (Crawler.AltTypes.ModuleInfo s1 s2 s3) =
    Crawler.Types.ModuleInfo s1 s2 s3

  toFuncInfos :: [AltFunctionInfo] -> [FunctionInfo]
  toFuncInfos altFuncInfos = map toFuncInfo altFuncInfos

  toFuncInfo :: AltFunctionInfo -> FunctionInfo
  toFuncInfo (Crawler.AltTypes.FunctionInfo a te b c d e)
      = Crawler.Types.FunctionInfo a (adjustTypeExpr te) b c d e
  
  toTypeInfo :: AltTypeInfo -> TypeInfo
  toTypeInfo (Crawler.AltTypes.TypeInfo a constr b c d e) =
    Crawler.Types.TypeInfo a (toConstructors constr) b c d e

  -- Transforms the constructors, by changing the TypeExpr to the normal format
  toConstructors :: [(Crawler.AltTypes.QName,
                      [Crawler.AltTypes.TypeExpr])]
                 -> [(FlatCurry.Types.QName, [FlatCurry.Types.TypeExpr])]
  toConstructors constrs = map (\(x, t) -> (x, map adjustTypeExpr t)) constrs

  -- Changes an alternative TypeExpr into a normal TypeExpr
  -- by changing the ForallTypes structure.
  adjustTypeExpr :: Crawler.AltTypes.TypeExpr -> FlatCurry.Types.TypeExpr
  adjustTypeExpr (Crawler.AltTypes.TVar a) =
    FlatCurry.Types.TVar a
  adjustTypeExpr (Crawler.AltTypes.FuncType t1 t2) =
    FlatCurry.Types.FuncType (adjustTypeExpr t1) (adjustTypeExpr t2)
  adjustTypeExpr (Crawler.AltTypes.TCons a ts) =
    FlatCurry.Types.TCons a (map adjustTypeExpr ts)
  adjustTypeExpr (Crawler.AltTypes.ForallType tVarIndexs t) =
    FlatCurry.Types.ForallType (map adjustTVarIndex tVarIndexs)
                               (adjustTypeExpr t)

  adjustTVarIndex :: Crawler.AltTypes.TVarIndex -> TVarWithKind
  adjustTVarIndex x = (x, KStar)