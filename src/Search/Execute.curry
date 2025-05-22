------------------------------------------------------------------------------
--- This module provides operations to execute search queries on a
--- Currygle index.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
------------------------------------------------------------------------------

module Search.Execute
  ( currygleSearch, profilingCurrygleSearch )
 where

import Data.List
import Data.Maybe
import System.CPUTime ( getCPUTime )

import Data.Map
import FlatCurry.FlexRigid

import Index.Helper        ( toLowerS )
import Index.Indexer
import Index.Trie
import Index.Item
import Index.Signature hiding (Function, Type)
import Search.Query
import Settings

--- Runs a earch query on a given index and returns the found items together
--- with a score for each item (lower scores are better matchings).
currygleSearch :: Index -> SearchQuery -> [(IndexItem,Int)]
currygleSearch index sq = toIndexItems index (search sq index)

-- Converts a search result into a list of index items sorted by their
-- match score, using the index as a translation.
toIndexItems :: Index -> Map Int Int -> [(IndexItem, Int)]
toIndexItems (Index items _ _ _ _ _ _ _ _ _ _) scores =
  sortBy (\(_,x1) (_,x2) -> x1<x2)
         (concatMap (\(ipos,score) -> case Data.Map.lookup ipos items of
                                        Nothing   -> []
                                        Just item -> [(item, score)])
                    (toList scores))

--- Runs a earch query on a given index and returns the found items together
--- with a score for each item (lower scores are better matchings).
--- Moreover, the elapsed time of the search is returned in the second
--- component.
profilingCurrygleSearch :: Index -> SearchQuery -> IO ([(IndexItem,Int)],Int)
profilingCurrygleSearch index sq = do
  starttime <- getCPUTime
  results   <- return $!! currygleSearch index sq
  stoptime  <- getCPUTime
  return (results, stoptime - starttime)

-- Runs a given search query on the index and returns a map from
-- index items (positions) and the score for this item.
search :: SearchQuery -> Index -> Map Int Int
search (Single st) index   = searchForTerm st index
-- For an AND query, intersect the results and use the worse match score
-- (i.e., the higher one) as the score of the intersected results.
search (AND sq1 sq2) index =
  intersectionWith max (search sq1 index) (search sq2 index)
-- For an OR query, compute the union and use the better match score
-- (i.e., the lower one) as the score of the unified results.
search (OR sq1 sq2) index  =
  unionWith min (search sq1 index) (search sq2 index)
-- For a NOT q, compute the difference:
search (NOT sq1 sq2) index = difference (search sq1 index) (search sq2 index)

-- Searches with a given single search term in the index and returns a map from
-- index items (positions) and the score for this item.
-- We adjust the score for description items so that items found in
-- descriptions are shown below other items, like entities names.
searchForTerm :: SearchTerm -> Index -> Map Int Int
searchForTerm (Description st) (Index _ descr _ _ _ _ _ _ _ _ _) =
  -- adjust score of description search so that they are shown later:
  mapWithKey (\_ v -> (v+5)) (textSearch descr (toLowerS st))
searchForTerm (Module st) (Index items _ modName _ _ _ _ _ _ _ _) =
  filterModuleResults items (trieSearch modName (toLowerS st))
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

-- Gets a map, a function to filter out elements, and a function to change
-- the values. First all values get filtered by the filter function,
-- then the other function is applied on them
cleanMap :: Ord a => Map a b -> (b -> Bool) -> (b -> c) -> Map a c
cleanMap m fil fun =
  fromList (map (\(x,y) -> (x, fun y))
                (filter (\(_,y) -> fil y)
                        (toList m)))

-- Remove in a result map all results which do not refer to module items
-- in the given index.
filterModuleResults :: Map Int IndexItem -> Map Int Int -> Map Int Int
filterModuleResults items results =
  filterWithKey (\pos _ -> isModuleItem (Data.Map.lookup pos items)) results
 where
  isModuleItem Nothing                 = False -- should not occur
  isModuleItem (Just (ModuleItem _ _ _ _))         = True
  isModuleItem (Just (FunctionItem _ _ _ _ _ _ _)) = False
  isModuleItem (Just (TypeItem _ _ _ _ _ _))       = False

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
-- The key of the map is the position of the found element in the IndexItem
-- list, and the value is the match score, the higher the worse.
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

-- Is the FlexRigidResult flexible?
matchFlex :: FlexRigidResult -> Bool
matchFlex KnownRigid = False
matchFlex KnownFlex  = True
matchFlex ConflictFR = False
matchFlex UnknownFR  = False

-- Is the FlexRigidResult rigid?
matchRigid :: FlexRigidResult -> Bool
matchRigid KnownRigid = True
matchRigid KnownFlex  = False
matchRigid ConflictFR = False
matchRigid UnknownFR  = False

------------------------------------------------------------------------------
