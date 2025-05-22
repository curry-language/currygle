----------------------------------------------------------------------
--- This module contains the definition of the Currygle index
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
import Index.Trie
import Index.Item
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
data Index = Index (Map Int IndexItem)
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
  Index (fromList nitems)
        (addIndexItemsToTrie descriptionOfItem nitems)
        (addIndexItemsToTrie moduleOfItem        nitems)
        (addIndexItemsToTrie packageOfItem       nitems)
        (addIndexItemsToTrie functionNamesOfItem nitems)
        (addIndexItemsToTrie typeNameOfItem      nitems)
        (addIndexItemsToTrie classNameOfItem     nitems)
        (addIndexItemsToTrie authorOfItem        nitems)
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
  cDetMap ((_,ModuleItem _ _ _ _):iis) = cDetMap iis
  cDetMap ((_,TypeItem _ _ _ _ _ _):iis)   = cDetMap iis
  cDetMap ((x,FunctionItem _ _ _ _ det _ _):iis) =
    insert x det (cDetMap iis)

-- Creates a map, where a Int representing the position of a FunctionItem in the IndexItem list
-- points to a FlexRigidResult telling if the function is flexible or not
createFlexMap :: [(Int,IndexItem)] -> Map Int FlexRigidResult
createFlexMap items = cFlexMap items
 where
  cFlexMap []                     = Data.Map.empty
  cFlexMap ((_,ModuleItem _ _ _ _):iis) = cFlexMap iis
  cFlexMap ((_,TypeItem _ _ _ _ _ _):iis)   = cFlexMap iis
  cFlexMap ((x,FunctionItem _ _ _ _ _ flex _):iis) =
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
  isClassFunction (ModuleItem _ _ _ _) = False
  isClassFunction (TypeItem _ _ _ _ _ _)   = False
  isClassFunction (FunctionItem name _ _ _ _ _ _) = '#' `elem` name

-- Stores an index in a directory, where the directory is created
-- if it does not exist yet, by writing all parts of the index into
-- seperate files.
writeIndex :: Index -> String -> IO ()
writeIndex (Index items descrs mods packs funcs types classes
                  authors det flex sigs) path = do
  createDirectoryIfMissing True path
  exists <- doesDirectoryExist (path)
  if not exists then createDirectory (path) else pure ()
  writeFile (path </> indexItemFileName)     (showData items)
  writeFile (path </> descrTrieFileName)     (showData descrs)
  writeFile (path </> moduleTrieFileName)    (showData mods)
  writeFile (path </> packageTrieFileName)   (showData packs)
  writeFile (path </> functionTrieFileName)  (showData funcs)
  writeFile (path </> typeTrieFileName)      (showData types)
  writeFile (path </> classTrieFileName)     (showData classes)
  writeFile (path </> authorTrieFileName)    (showData authors)
  writeFile (path </> detMapFileName)        (showData det)
  writeFile (path </> flexMapFileName)       (showData flex)
  writeFile (path </> signatureTrieFileName) (showData sigs)
  putStrLn $ "Index with " ++ show (size items) ++ " items written."

-- Gets the path to an index directory as a string, and returns the stored index
readIndex :: String -> IO Index
readIndex indexPath = do
  items  <- fmap mbEmptyMap $ readDataFile (indexPath </> indexItemFileName)
  descrs <- fmap mbEmptyTrie $ readDataFile (indexPath </> descrTrieFileName)
  mods   <- fmap mbEmptyTrie $ readDataFile (indexPath </> moduleTrieFileName)
  packs  <- fmap mbEmptyTrie $ readDataFile (indexPath </> packageTrieFileName)
  funcs  <- fmap mbEmptyTrie $ readDataFile (indexPath </> functionTrieFileName)
  types  <- fmap mbEmptyTrie $ readDataFile (indexPath </> typeTrieFileName)
  classes <- fmap mbEmptyTrie $ readDataFile (indexPath </> classTrieFileName)
  authors <- fmap mbEmptyTrie $ readDataFile (indexPath </> authorTrieFileName)
  det  <- fmap mbEmptyMap $ readDataFile (indexPath </> detMapFileName)
  flex <- fmap mbEmptyMap $ readDataFile (indexPath </> flexMapFileName)
  sigs <- fmap mbEmptyTrie $ readDataFile (indexPath </> signatureTrieFileName)
  return $ Index items descrs mods packs funcs types classes
                  authors det flex sigs
 where
  mbEmptyMap  mb = maybe Data.Map.empty id mb
  mbEmptyTrie mb = maybe emptyTrie id mb

------------------------------------------------------------------------------
