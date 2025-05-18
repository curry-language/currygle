----------------------------------------------------------------------
--- This module provides the assignMatchScore function, which assigns
--- a list of IndexItems a score how well they match a searchQuery.
--- This is the core search functionality of currygle2
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module Search.Search ( currygleSearch, profilingCurrygleSearch )
    where

import Data.List
import Data.Maybe

import Data.Map
import FlatCurry.FlexRigid
import Debug.Profile       ( getElapsedTimeNF )
import Index.Indexer
import Index.IndexTrie
import Index.IndexItem
import Index.Signature hiding (Function, Type)
import Search.SearchQuery
import Settings

--- Search query in the given index.
currygleSearch :: Index -> SearchQuery -> [(IndexItem,Int)]
currygleSearch index sq = toIndexItems index (search sq index)

--- Search query in the given index and return the results together with
--- the elapsed time of the search.
profilingCurrygleSearch :: Index -> SearchQuery -> IO ([(IndexItem,Int)],Int)
profilingCurrygleSearch index sq =
  getElapsedTimeNF (return $ currygleSearch index sq)

-- Searches with a given search query in the index and returns a map from
-- index items (positions) and the score for this item.
search :: SearchQuery -> Index -> Map Int Int
search (Single st) index   = searchForTerm st index
-- For AND searchQuery, do a intersection, and use the worse matchscore (the higher one)
search (AND sq1 sq2) index =
  intersectionWith (\x y -> if x < y then y else x)
                   (search sq1 index) (search sq2 index)
-- For OR searchQuery, do a union, and use the better matchscore (the lower one)
search (OR sq1 sq2) index  =
  unionWith (\x y -> if x < y then x else y)
            (search sq1 index) (search sq2 index)
-- For NOT searchQuery, do a difference
search (NOT sq1 sq2) index = difference (search sq1 index) (search sq2 index)

-- Converts a search result into a list of index items sorted by their
-- match score, using the index as a translation.
toIndexItems :: Index -> Map Int Int -> [(IndexItem, Int)]
toIndexItems (Index items _ _ _ _ _ _ _ _ _ _) scores =
  sortBy (\(_,x1) (_,x2) -> x1<x2) (toIndexItemsRec items scores 0)
 where
  toIndexItemsRec :: [IndexItem] -> Map Int Int -> Int -> [(IndexItem, Int)]
  toIndexItemsRec []     _      _ = []
  toIndexItemsRec (x:xs) resmap n = case Data.Map.lookup n resmap of
    Nothing -> toIndexItemsRec xs resmap (n+1)
    Just i  -> (x, i) : toIndexItemsRec xs resmap (n+1)

-- Searches with a given single search term in the index and returns a map from
-- index items (positions) and the score for this item.
-- We adjust the score for description items so that items found in
-- descriptions are shown below other items, like entities names.
searchForTerm :: SearchTerm -> Index -> Map Int Int
searchForTerm (Description st) (Index _ descr _ _ _ _ _ _ _ _ _) =
  -- adjust score of description search:
  mapWithKey (\_ v -> (v+5)) (textSearch descr (toLowerS st))
searchForTerm (Module st) (Index items _ modName _ _ _ _ _ _ _ _) =
  filterForModule items (trieSearch modName (toLowerS st))
searchForTerm (InModule st) (Index _ _ modName _ _ _ _ _ _ _ _) =
  textSearch modName (toLowerS st)
searchForTerm (InPackage st) (Index _ _ _ packName _ _ _ _ _ _ _) =
  textSearch packName (toLowerS st)
searchForTerm (Function st) (Index _ _ _ _ fun _ _ _ _ _ _) =
  textSearch fun (toLowerS st)
searchForTerm (Type st) (Index _ _ _ _ _ t _ _ _ _ _) =
  textSearch t (toLowerS st)
searchForTerm (Class st) (Index _ _ _ _ _ _ c _ _ _ _) =
  textSearch c (toLowerS st)
searchForTerm (Author st) (Index _ _ _ _ _ _ _ author _ _ _) =
  textSearch author st
searchForTerm Det (Index _ _ _ _ _ _ _ _ det _ _) =
  cleanMap det (\x -> x) (\_ -> 0)
searchForTerm NonDet (Index _ _ _ _ _ _ _ _ det _ _) =
  cleanMap det (\x -> not x) (\_ -> 0)
searchForTerm Flexible (Index _ _ _ _ _ _ _ _ _ flex _) =
  cleanMap flex matchFlex (\_ -> 0)
searchForTerm Rigid (Index _ _ _ _ _ _ _ _ _ flex _) =
  cleanMap flex matchRigid (\_ -> 0)
searchForTerm (Signature st) (Index _ _ _ _ _ _ _ _ _ _ sigs) =
  case st of Nothing -> Data.Map.empty
             Just x  -> trieSearch sigs (seperateSig x)
searchForTerm (All st) index =
  search (OR (OR (Single (Function st))
                 (Single (Type st)))
             (OR (Single (Module st))
                 (OR (Single (Class st))
                     (Single (Description st)))))
         index

-- Gets a map, a function to filter out elements, and a function to change the values.
-- First all values get filtered by the filter function, then the other function is applied on them
cleanMap :: Ord a => Map a b -> (b -> Bool) -> (b -> c) -> Map a c
cleanMap m fil fun = fromList
                            (map (\(x,y) -> (x, fun y))
                            (filter (\(_,y) -> fil y)
                            (toList m)))

-- Gets the complete list of IndexItems, and deletes all which are not modules in the map.
filterForModule :: [IndexItem] -> Map Int Int -> Map Int Int
filterForModule items toBeFiltered = filterForModuleAcc items toBeFiltered 0
where
    filterForModuleAcc :: [IndexItem] -> Map Int Int -> Int -> Map Int Int
    filterForModuleAcc [] left _ = left
    filterForModuleAcc ((ModuleItem _):is) left x = filterForModuleAcc is left (x+1)
    filterForModuleAcc ((FunctionItem _):is) left x = filterForModuleAcc is (Data.Map.delete x left) (x+1)
    filterForModuleAcc ((TypeItem _):is) left x = filterForModuleAcc is (Data.Map.delete x left) (x+1)

-- Searches for a String in a Trie, and returns a map with the result Map.
-- The key of the map is the position of the found element in the IndexItem list
-- and the value is the match score, the higher the worse.
textSearch :: Trie Char (Int, Int) -> String -> Map Int Int
textSearch trie searchTerm =
  mapWithKey (\_ v -> v - (length searchTerm)) (trieTextSearch trie searchTerm)

trieTextSearch :: Trie Char (Int,Int) -> String -> Map Int Int
trieTextSearch (Node _         values) []     = fromList values
trieTextSearch (Node subTries  _)      (t:ts) =
  if member t subTries
    then let Just trie = Data.Map.lookup t subTries
         in trieTextSearch trie ts
    else Data.Map.empty


-- Searches for a key in a Trie, and returns a result Map.
-- The key of the map is the position of the found element in the IndexItem list, and
-- the value is the matchscore, the higher the worse.
trieSearch :: Ord k => Trie k (Int, Int) -> [k] -> Map Int Int
trieSearch trie searchTerm =
  mapWithKey (\_ v -> v - (length searchTerm)) (trieKeySearch trie searchTerm)

trieKeySearch :: Ord k => Trie k (Int,Int) -> [k] -> Map Int Int
trieKeySearch (Node _         values) []     = fromList values
trieKeySearch (Node subTries  _)      (t:ts) 
    =   if member t subTries then   
            let Just trie = Data.Map.lookup t subTries in
                trieKeySearch trie ts
        else
            Data.Map.empty

-- Returns 1 if the FlexRigidResult is known to be flexible, otherwise returns 0
matchFlex :: FlexRigidResult -> Bool
matchFlex KnownRigid = False
matchFlex KnownFlex = True
matchFlex ConflictFR = False
matchFlex UnknownFR = False

-- Returns 1 if the FlexRigidResult is known to be rigid, otherwise returns 0
matchRigid :: FlexRigidResult -> Bool
matchRigid KnownRigid = True
matchRigid KnownFlex = False
matchRigid ConflictFR = False
matchRigid UnknownFR = False