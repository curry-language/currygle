{-# OPTIONS_FRONTEND -Wno-incomplete-patterns -Wno-unused-bindings #-}

module Index.IndexTrie
    where

import Data.Maybe       ( isNothing )

import FlatCurry.Types
import FlatCurry.FlexRigid

import Data.Map

import RW.Base

import Index.Helper
import Index.IndexItem
import Index.Signature

-- A Trie, a type of searchtree where you can search by a list of type [k],
-- and it returns all values of type v stored under the key in the tree.
data Trie k v = Node (Map k (Trie k v)) [v]
    deriving (Show, Read)

emptyTrie :: Trie k v
emptyTrie = Node (Data.Map.empty) []

-- Adds an element of type v into the list stored under the key [k]
addToTrie :: Ord k => Trie k v -> [k] -> v -> Trie k v
-- If we found the key, so the key list is empty, add the new value to the current Trie
addToTrie (Node m vs) [] v  = Node m (v:vs)
addToTrie (Node m vs) (k:ks) v  | member k m = let Just subTrie = (Data.Map.lookup k m) in
                                                    Node (insert k (addToTrie subTrie ks v) m) vs
                                | otherwise  = Node (insert k (addToTrie emptyTrie ks v) m) vs

addListToTrie :: Ord k => Trie k v -> [([k], v)] -> Trie k v
addListToTrie trie [] = trie
addListToTrie trie ((key, value):xs) = addListToTrie (addToTrie trie key value) xs

------------------------------------------------
-- Section for creation of the different IndexTrees
------------------------------------------------

-- Creates a trie storing the name of the module for all IndexItems in the list
createDescrTrie :: [IndexItem] -> Trie Char (Int,Int)
createDescrTrie = addIndexMItemsToTree descriptionOfItem emptyTrie 0

-- Creates a trie storing the name of the module for all IndexItems in the list
createModuleTrie :: [IndexItem] -> Trie Char (Int,Int)
createModuleTrie indexItems = addIndexItemsToTree getModule emptyTrie indexItems 0

-- Creates a trie storing the name of the package for all IndexItems in the list
createPackageTrie :: [IndexItem] -> Trie Char (Int,Int)
createPackageTrie items = addIndexItemsToTree getPackage emptyTrie items 0

-- Creates a trie storing the name of the function for all FunctionIndex in the list
createFunctionTrie :: [IndexItem] -> Trie Char (Int,Int)
createFunctionTrie items = addIndexItemsToTree getFunctionName emptyTrie items 0

-- Creates a trie storing the name of the type for all TypeIndex in the list, which are not classes
createTypeTrie :: [IndexItem] -> Trie Char (Int,Int)
createTypeTrie items = addIndexItemsToTree getTypeName emptyTrie items 0

-- Creates a trie storing the name of the class for all TypeIndex in the list, which are classes
createClassTrie :: [IndexItem] -> Trie Char (Int,Int)
createClassTrie items = addIndexItemsToTree getClassName emptyTrie items 0

-- Creates a trie storing the name of the author for all IndexItems in the list
createAuthorTrie :: [IndexItem] -> Trie Char (Int,Int)
createAuthorTrie items = addIndexItemsToTree getAuthor emptyTrie items 0

-- Creates a trie storing the signature for all TypeIndex and FunctionIndex in the list
createSignatureTrie :: [IndexItem] -> Trie Signature (Int,Int)
createSignatureTrie items = createSignatureTrieRec items 0
  where
    createSignatureTrieRec :: [IndexItem] -> Int -> Trie Signature (Int, Int)
    createSignatureTrieRec []     n = emptyTrie
    createSignatureTrieRec (i:is) n = case getSignatures i of
      Nothing -> createSignatureTrieRec is (n+1)
      Just sigs -> addAllSignatures (createSignatureTrieRec is (n+1)) sigs n

    addAllSignatures :: Trie Signature (Int, Int) -> [[Signature]] -> Int -> Trie Signature (Int, Int)
    addAllSignatures t []     i = t
    addAllSignatures t (s:ss) i = addAllSignatures (addItemToTrie t s (i, length s)) ss i

-- Gets a function to get the key from the IndexItem,
-- a Trie, which should be the empty Trie,
-- a list of IndexItems,
-- and an int, which should be 0 by default.
-- Adds the position of all IndexItems in the list to the Trie under all
-- suffixes as keys
addIndexItemsToTree :: Ord k => (IndexItem -> Maybe [k]) -> Trie k (Int,Int)
                    -> [IndexItem] -> Int -> Trie k (Int,Int)
addIndexItemsToTree _ trie []       _       = trie
addIndexItemsToTree f trie (ii:iis) counter = case f ii of
  Nothing -> addIndexItemsToTree f trie iis (counter+1)
  Just l  -> addIndexItemsToTree f (addItemToTrie trie l (counter, length l))
                                 iis (counter+1)

-- Gets a Trie, a key and an item to add.
-- Adds the item into a Trie under all sublist of the key,
-- including the empty one.
addItemToTrie :: Ord k => Trie k v -> [k] -> v -> Trie k v
addItemToTrie trie str i =
  addListToTrie trie 
                (combineKeysWithPos
                  (concat (map getAllPrefixes (getAllSuffixes str)) ++ [[]])
                  i)

-- For a function which extracts a list of keys from the `IndexItem`,
-- a trie, which should be the empty Trie,
-- a list of IndexItems,
-- and an int, which should be 0 by default,
-- add the position of all IndexItems in the list to the Trie under all
-- suffixes as keys.
addIndexMItemsToTree :: Ord k => (IndexItem -> [[k]]) -> Trie k (Int,Int)
                     -> Int -> [IndexItem] -> Trie k (Int,Int)
addIndexMItemsToTree _ trie _       []       = trie
addIndexMItemsToTree f trie counter (ii:iis) = case f ii of
  [] -> addIndexMItemsToTree f trie (counter+1) iis
  ks -> addIndexMItemsToTree f (addItemsToTrie trie ks (counter, length ks))
                               (counter+1) iis

-- Adds a list of keys for a given item into a trie.
addItemsToTrie :: Ord k => Trie k v -> [[k]] -> v -> Trie k v
addItemsToTrie trie keys i = foldr (\k t -> addItemToTrie t k i) trie keys

-- Gets a string, and returns a list containing all sufffixes of that String
getAllSuffixes :: [a] -> [[a]]
getAllSuffixes []       = []
getAllSuffixes (c:cs)   = (c:cs):(getAllSuffixes cs)

-- Gets a string, and returns a list containing all prefixes of that String
getAllPrefixes :: [a] -> [[a]]
getAllPrefixes []     = []
getAllPrefixes (x:xs) = getAllPrefixesAcc [x] xs [[x]]
  where
    -- 1. The previous prefix
    -- 2. The leftover
    -- 3. The list of all previous prefixes
    -- Returns a list of all prefixes
    getAllPrefixesAcc :: [a] -> [a] -> [[a]] -> [[a]]
    getAllPrefixesAcc _    []     acc = acc
    getAllPrefixesAcc prev (l:ls) acc = getAllPrefixesAcc (prev++[l]) ls ((prev++[l]):acc)

combineKeysWithPos :: [[a]] -> b -> [([a], b)]
combineKeysWithPos []           _ = []
combineKeysWithPos (str:strs)   x = (str,x):(combineKeysWithPos strs x)

-- Auto generated code from rw-data

instance (ReadWrite a,ReadWrite b) => ReadWrite (Trie a b) where
  readRW strs r0 = (Node a' b',r2)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1

  showRW params strs0 (Node a' b') = (strs2,show1 . show2)
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'

  writeRW params h (Node a' b') strs =
    writeRW params h a' strs >>= writeRW params h b'

  typeOf n = RWType "Trie" [typeOf (get_a n),typeOf (get_b n)]
    where
      get_a :: Trie a' b' -> a'
      get_a _ = failed
      get_b :: Trie a' b' -> b'
      get_b _ = failed