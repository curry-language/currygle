----------------------------------------------------------------------
--- This module contains the definition of the index for Currygle
--- and operations to read and write the index in a compact form.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module Index.Indexer
  ( Index(..), createIndexFromDir, writeIndex, readIndex )
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

-- The Currygle index consists of a lookup table of all items in the index
-- and Tries for quick search. various entities.
-- The values of the tries contain the index position of the corresponding
-- item and the matching score.
--
-- 0. [IndexItem] is the list of all IndexItems in the index, where the tries
--    refer to by the index position
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
        (addIndexItemsToTrie descriptionOfItem nitems)
        (addIndexItemsToTrie getModule         nitems)
        (addIndexItemsToTrie getPackage        nitems)
        (addIndexItemsToTrie getFunctionNames  nitems)
        (addIndexItemsToTrie getTypeName       nitems)
        (addIndexItemsToTrie getClassName      nitems)
        (addIndexItemsToTrie getAuthor         nitems)
        (createDetMap nitems)
        (createFlexMap nitems)
        (createSignatureTrie nitems)
 where nitems = zip [0..] items

-- Creates a map, where a Int representing the position of a FunctionItem
-- in the IndexItem list points to a Bool which is `True` if the function
-- is deterministic.
createDetMap :: [(Int,IndexItem)] -> Map Int Bool
createDetMap items = cDetMap items
 where
  cDetMap []                     = Data.Map.empty
  cDetMap ((_,ModuleItem _):iis) = cDetMap iis
  cDetMap ((_,TypeItem _):iis)   = cDetMap iis
  cDetMap ((x,FunctionItem (FunctionIndex _ _ _ _ det _ _ _)):iis) =
    insert x det (cDetMap iis)

-- Creates a map, where a Int representing the position of a FunctionItem in the IndexItem list
-- points to a FlexRigidResult telling if the function is flexible or not
createFlexMap :: [(Int,IndexItem)] -> Map Int FlexRigidResult
createFlexMap items = cFlexMap items
 where
  cFlexMap []                     = Data.Map.empty
  cFlexMap ((_,ModuleItem _):iis) = cFlexMap iis
  cFlexMap ((_,TypeItem _):iis)   = cFlexMap iis
  cFlexMap ((x,FunctionItem (FunctionIndex _ _ _ _ _ flex _ _)):iis) =
    insert x flex (cFlexMap iis)

-- Creates an index from `.cdoc` files (as produced by CurryDoc) stored in
-- the directory provided as the argument.
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
  putStrLn $ "Index with " ++ show (length items) ++ " items written."

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

------------------------------------------------------------------------------
