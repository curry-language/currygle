----------------------------------------------------------------------
--- A module to write the index for Currygle2.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module Index.Indexer
  where

import Data.Map
import FlatCurry.FlexRigid
import FlatCurry.Types
import RW.Base
import System.Directory
import System.FilePath  ( (</>) )

import Crawler.Crawler
import Crawler.Readers
import Index.IndexTrie
import Index.IndexItem
import Index.Signature
import Settings

-- An index consists of a lookup table of all items in the index, and Tries for quick search.
-- 0. [IndexItem] is the List of all IndexItems in the index, which are referenced in the Tries
-- 1. Trie: names occurring in descriptions of all index items
-- 2. Trie: module name for all index items, for :module and :inModule search
-- 3. Trie: package names, for inPackage search
-- 4. Trie: function names, for :function search
-- 5. Trie: type names, for :type search
-- 6. Trie: class names, for :class search
-- 7. Trie: author names, for :author search
-- 8. Map : mapping the functions to deterministic (True) or not (False)
-- 9. Map : mapping the functions to flexible (True) or rigid (False)
-- 10. Trie: Signatures, 
data Index = Index [IndexItem] 
                   (Trie Char (Int,Int))
                   (Trie Char (Int,Int))
                   (Trie Char (Int,Int))
                   (Trie Char (Int,Int)) 
                   (Trie Char (Int,Int))
                   (Trie Char (Int,Int))
                   (Trie Char (Int,Int))
                   (Map Int Bool)
                   (Map Int FlexRigidResult)
                   (Trie Signature (Int, Int))
  deriving (Show, Read)

--- Creates the Currygle search index for the given items.
createIndex :: [IndexItem] -> Index
createIndex items =
  Index items
        (createDescrTrie items)
        (createModuleTrie items)
        (createPackageTrie items)
        (createFunctionTrie items)
        (createTypeTrie items)
        (createClassTrie items)
        (createAuthorTrie items)
        (createDetMap items)
        (createFlexMap items)
        (createSignatureTrie items)

-- Creates a map, where a Int representing the position of a FunctionItem
-- in the IndexItem list points to a Bool telling if the function
-- is deterministic.
createDetMap :: [IndexItem] -> Map Int Bool
createDetMap items = createDetMapRec items 0
  where
    createDetMapRec :: [IndexItem] -> Int -> Map Int Bool
    createDetMapRec []                                                   _ = Data.Map.empty
    createDetMapRec ((ModuleItem _):iis)                                 x = createDetMapRec iis (x+1)
    createDetMapRec ((TypeItem _):iis)                                   x = createDetMapRec iis (x+1)
    createDetMapRec ((FunctionItem (FunctionIndex _ _ _ _ det _ _ _)):iis) x = insert x det (createDetMapRec iis (x+1))

-- Creates a map, where a Int representing the position of a FunctionItem in the IndexItem list
-- points to a FlexRigidResult telling if the function is flexible or not
createFlexMap :: [IndexItem] -> Map Int FlexRigidResult
createFlexMap items = createFlexMapRec items 0
  where
    createFlexMapRec :: [IndexItem] -> Int -> Map Int FlexRigidResult
    createFlexMapRec []                                                    _ = Data.Map.empty
    createFlexMapRec ((ModuleItem _):iis)                                  x = createFlexMapRec iis (x+1)
    createFlexMapRec ((TypeItem _):iis)                                    x = createFlexMapRec iis (x+1)
    createFlexMapRec ((FunctionItem (FunctionIndex _ _ _ _ _ flex _ _)):iis) x = insert x flex (createFlexMapRec iis (x+1))

-- Gets a String, which is the path to the directory containing the cdoc files, and only cdoc file.
-- Gets a second String, which is the path where the finished Index will be stored.
-- Creates a simple index by writing a list of all CDocs into an txt file.
createIndexFromDir :: String -> IO Index
createIndexFromDir cdocdir = do
  cipkgs <- getAllCurryInfo cdocdir
  putStrLn "Creating index structures..."
  let allitems = concatMap toIndexItem cipkgs
      items    = filter (not . isClassFunction) allitems
  return $ createIndex items
 where
  isClassFunction :: IndexItem -> Bool
  isClassFunction (ModuleItem _) = False
  isClassFunction (TypeItem _)   = False
  isClassFunction (FunctionItem (FunctionIndex name _ _ _ _ _ _ _)) =
    '#' `elem` name

-- Stores an index in a directory, where the directory is created
-- if it does not exist yet, by writing all parts of the index into
-- seperate files.
writeIndex :: Index -> String -> IO ()
writeIndex (Index items descrs mods packs funcs types classes
                  authors det flex sigs) path = do
  createDirectoryIfMissing True path
  exists <- doesDirectoryExist (path)
  if not exists then createDirectory (path) else pure ()
  writeFile (path </> indexItemFileName) (showData items)
  writeFile (path </> descrTrieFileName) (showData descrs)
  writeFile (path </> moduleTrieFileName) (showData mods)
  writeFile (path </> packageTrieFileName) (showData packs)
  writeFile (path </> functionTrieFileName) (showData funcs)
  writeFile (path </> typeTrieFileName) (showData types)
  writeFile (path </> classTrieFileName) (showData classes)
  writeFile (path </> authorTrieFileName) (showData authors)
  writeFile (path </> detMapFileName) (showData det)
  writeFile (path </> flexMapFileName) (showData flex)
  writeFile (path </> signatureTrieFileName) (showData sigs)

-- Gets the path to an index directory as a string, and returns the stored index
readIndex :: String -> IO Index
readIndex indexPath = do
  itemsM <- readDataFile (indexPath </> indexItemFileName)
  items <- case itemsM of
              Nothing -> return []
              Just a  -> return a
  descrsM <- readDataFile (indexPath </> descrTrieFileName)
  descrs <- case descrsM of
              Nothing -> return (emptyTrie :: Trie Char (Int,Int))
              Just a  -> return a
  modsM <- readDataFile (indexPath </> moduleTrieFileName)
  mods <- case modsM of
              Nothing -> return (emptyTrie :: Trie Char (Int,Int))
              Just a  -> return a
  packsM <- readDataFile (indexPath </> packageTrieFileName)
  packs <- case packsM of
              Nothing -> return (emptyTrie :: Trie Char (Int,Int))
              Just a  -> return a
  funcsM <- readDataFile (indexPath </> functionTrieFileName)
  funcs <- case funcsM of
              Nothing -> return (emptyTrie :: Trie Char (Int,Int))
              Just a  -> return a
  typesM <- readDataFile (indexPath </> typeTrieFileName)
  types <- case typesM of
              Nothing -> return (emptyTrie :: Trie Char (Int,Int))
              Just a  -> return a
  classesM <- readDataFile (indexPath </> classTrieFileName)
  classes <- case classesM of
              Nothing -> return (emptyTrie :: Trie Char (Int,Int))
              Just a  -> return a
  authorsM <- readDataFile (indexPath </> authorTrieFileName)
  authors <- case authorsM of
              Nothing -> return (emptyTrie :: Trie Char (Int,Int))
              Just a  -> return a
  detM <- readDataFile (indexPath </> detMapFileName)
  det <- case detM of
              Nothing -> return (Data.Map.empty :: Map Int Bool)
              Just a  -> return a
  flexM <- readDataFile (indexPath </> flexMapFileName)
  flex <- case flexM of
              Nothing -> return (Data.Map.empty :: Map Int FlexRigidResult)
              Just a  -> return a
  sigsM <- readDataFile (indexPath </> signatureTrieFileName)
  sigs <- case sigsM of
              Nothing -> return (emptyTrie :: Trie Signature (Int, Int))
              Just a  -> return a
  return $ Index items descrs mods packs funcs types classes
                  authors det flex sigs
