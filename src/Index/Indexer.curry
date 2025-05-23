----------------------------------------------------------------------
--- This module contains the definition of the Currygle index
--- and operations to read and write the index in a compact form.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module Index.Indexer
  ( Index(..), createIndexFromDir, writeIndex, readIndex, readIndexStrict )
 where

import Data.Map
import FlatCurry.FlexRigid
import FlatCurry.Types
import RW.Base              ( readDataFile, writeDataFile )
import System.Directory     ( createDirectoryIfMissing, doesDirectoryExist )
import System.FilePath      ( (</>) )

import Crawler.Crawler
import Crawler.Readers
import Index.Trie
import Index.Item
import Index.Signature
import Options
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
-- separate files.
writeIndex :: Options -> FilePath -> Index -> IO ()
writeIndex opts indexpath
  (Index items descrs mods packs funcs types classes authors det flex sigs) = do
  createDirectoryIfMissing True indexpath
  writeIndexFile indexItemFileName     items
  writeIndexFile descrTrieFileName     descrs
  writeIndexFile moduleTrieFileName    mods
  writeIndexFile packageTrieFileName   packs
  writeIndexFile functionTrieFileName  funcs
  writeIndexFile typeTrieFileName      types
  writeIndexFile classTrieFileName     classes
  writeIndexFile authorTrieFileName    authors
  writeIndexFile detMapFileName        det
  writeIndexFile flexMapFileName       flex
  writeIndexFile signatureTrieFileName sigs
  putStrLn $ "Index with " ++ show (size items) ++ " items written."
 where
  writeIndexFile fn d = do
    printWhenStatus opts $ "Writing index file '" ++ fn ++ "''..."
    writeDataFile (indexpath </> fn) d

-- Reads the Currygle index from the argument directory.
readIndex :: String -> IO Index
readIndex indexpath = do
  items   <- fmap mbEmptyMap  $ readIndexFile indexItemFileName
  descrs  <- fmap mbEmptyTrie $ readIndexFile descrTrieFileName
  mods    <- fmap mbEmptyTrie $ readIndexFile moduleTrieFileName
  packs   <- fmap mbEmptyTrie $ readIndexFile packageTrieFileName
  funcs   <- fmap mbEmptyTrie $ readIndexFile functionTrieFileName
  types   <- fmap mbEmptyTrie $ readIndexFile typeTrieFileName
  classes <- fmap mbEmptyTrie $ readIndexFile classTrieFileName
  authors <- fmap mbEmptyTrie $ readIndexFile authorTrieFileName
  det     <- fmap mbEmptyMap  $ readIndexFile detMapFileName
  flex    <- fmap mbEmptyMap  $ readIndexFile flexMapFileName
  sigs    <- fmap mbEmptyTrie $ readIndexFile signatureTrieFileName
  return $ Index items descrs mods packs funcs types classes authors det flex
                 sigs
 where
  readIndexFile fn = readDataFile (indexpath </> fn)

  mbEmptyMap  mb = maybe Data.Map.empty id mb
  mbEmptyTrie mb = maybe emptyTrie id mb

-- Strictly reads the Currygle index from the argument directory.
readIndexStrict :: Options -> String -> IO Index
readIndexStrict opts indexpath = do
  (Index items0 descs0 mods0 pkgs0 funs0 typs0 cls0 auths0 det0 flex0 sigs0)
    <- readIndex indexpath
  items <- strictReadItems "item"        items0
  descs <- strictReadItems "description" descs0
  mods  <- strictReadItems "module"      mods0
  pkgs  <- strictReadItems "package"     pkgs0
  funs  <- strictReadItems "function"    funs0
  typs  <- strictReadItems "type"        typs0
  cls   <- strictReadItems "classe"      cls0
  auths <- strictReadItems "author"      auths0
  sigs  <- strictReadItems "signature"   sigs0
  det   <- strictReadItems "determinism" det0
  flex  <- strictReadItems "flexible"    flex0
  return (Index items descs mods pkgs funs typs cls auths det flex sigs)
 where
  strictReadItems descitems items = do
    printWhenStatus opts $ "Reading " ++ descitems ++ " index..."
    return $!! items

------------------------------------------------------------------------------
