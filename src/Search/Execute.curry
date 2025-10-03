------------------------------------------------------------------------------
--- This module provides operations to execute search queries on a
--- Currygle index.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version October 2025
------------------------------------------------------------------------------

module Search.Execute
  ( currygleSearch, profilingCurrygleSearch )
 where

import Data.List      ( groupBy, sortBy )
import System.CPUTime ( getCPUTime )

import Data.Map
import FlatCurry.FlexRigid

import Index.Helper    ( toLowerS )
import Index.Indexer
import Index.Trie
import Index.Item
import Index.Signature ( flattenSignature )
import Search.Query
import Settings

--- Runs a earch query on a given index and returns the found items together
--- with a score for each item (lower scores are better matchings).
--- If the first argument is `True`, fuzzy search is used.
currygleSearch :: Bool -> Index -> SearchQuery -> [(IndexItem,Int)]
currygleSearch fuzzy index sq =
  sortIndexItemsByUpload (toIndexItems index (search fuzzy sq index))

-- Sort index items with identical score by the age of their package.
sortIndexItemsByUpload :: [(IndexItem,Int)] -> [(IndexItem,Int)]
sortIndexItemsByUpload items =
  concatMap (sortBy (\(i1,_) (i2,_) -> uploadOfItem i1 > uploadOfItem i2)) $
    groupBy (\(_,s1) (_,s2) -> s1==s2) items

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
--- If the first argument is `True`, fuzzy search is used.
--- Moreover, the elapsed time of the search is returned in the second
--- component.
profilingCurrygleSearch :: Bool -> Index -> SearchQuery
                        -> IO ([(IndexItem,Int)],Int)
profilingCurrygleSearch fuzzy index sq = do
  starttime <- getCPUTime
  results   <- return $!! currygleSearch fuzzy index sq
  stoptime  <- getCPUTime
  return (results, stoptime - starttime)

--- Runs a given search query on the index and returns a map from
--- index items (positions) and the score for this item.
--- If the first argument is `True`, fuzzy search is used for text searches.
search :: Bool -> SearchQuery -> Index -> Map Int Int
search fuzzy searchquery index = searchCombQuery searchquery
 where
  searchCombQuery (Single st) = searchForTerm fuzzy st index
  -- For an AND query, intersect the results and use the worse match score
  -- (i.e., the higher one) as the score of the intersected results.
  searchCombQuery (AND sq1 sq2) =
    intersectionWith max (searchCombQuery sq1) (searchCombQuery sq2)
  -- For an OR query, compute the union and use the better match score
  -- (i.e., the lower one) as the score of the unified results.
  searchCombQuery (OR sq1 sq2)  =
    unionWith min (searchCombQuery sq1) (searchCombQuery sq2)
  -- For a NOT q, compute the difference:
  searchCombQuery (NOT sq1 sq2) =
    difference (searchCombQuery sq1) (searchCombQuery sq2)

-- Searches with a given single search term in the index and returns a map from
-- index items (positions) and the score for this item.
-- We adjust the score for description items so that items found in
-- descriptions are shown below other items, like entities names.
searchForTerm :: Bool -> SearchTerm -> Index -> Map Int Int
searchForTerm fuzzy (Description st) (Index _ descr _ _ _ _ _ _ _ _ _) =
  -- adjust score of description search so that they are shown later:
  mapWithKey (\_ v -> (v+5)) (textSearch fuzzy descr (toLowerS st))
searchForTerm _ (Module st) (Index items _ modName _ _ _ _ _ _ _ _) =
  filterModuleResults items (trieSearch modName (toLowerS st))
searchForTerm fuzzy (InModule st) (Index _ _ modName _ _ _ _ _ _ _ _) =
  textSearch fuzzy modName (toLowerS st)
searchForTerm fuzzy (InPackage st) (Index _ _ _ packName _ _ _ _ _ _ _) =
  textSearch fuzzy packName (toLowerS st)
searchForTerm fuzzy (Function st) (Index _ _ _ _ fun _ _ _ _ _ _) =
  textSearch fuzzy fun (toLowerS st)
searchForTerm fuzzy (Type st) (Index _ _ _ _ _ t _ _ _ _ _) =
  textSearch fuzzy t (toLowerS st)
searchForTerm fuzzy (Class st) (Index _ _ _ _ _ _ c _ _ _ _) =
  textSearch fuzzy c (toLowerS st)
searchForTerm fuzzy (Author st) (Index _ _ _ _ _ _ _ author _ _ _) =
  textSearch fuzzy author st
searchForTerm _ Det (Index _ _ _ _ _ _ _ _ det _ _) =
  cleanMap det (\x -> x) (\_ -> 0)
searchForTerm _ NonDet (Index _ _ _ _ _ _ _ _ det _ _) =
  cleanMap det (\x -> not x) (\_ -> 0)
searchForTerm _ Flexible (Index _ _ _ _ _ _ _ _ _ flex _) =
  cleanMap flex matchFlex (\_ -> 0)
searchForTerm _ Rigid (Index _ _ _ _ _ _ _ _ _ flex _) =
  cleanMap flex matchRigid (\_ -> 0)
searchForTerm _ (Signature st) (Index _ _ _ _ _ _ _ _ _ _ sigs) =
  case st of Nothing -> Data.Map.empty
             Just x  -> trieSearch sigs (flattenSignature x)
searchForTerm fuzzy (All st) index =
  search fuzzy (OR (OR (Single (Function st))
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
  isModuleItem (Just (ModuleItem _ _ _ _ _))         = True
  isModuleItem (Just (FunctionItem _ _ _ _ _ _ _ _)) = False
  isModuleItem (Just (TypeItem _ _ _ _ _ _ _))       = False

-- Switch between strict and fuzzy search:
textSearch :: Bool -> Trie Char (Int, Int) -> String -> Map Int Int
textSearch fuzzy = if fuzzy then fuzzyTextSearch else strictTextSearch

------------------------------------------------------------------------------
-- Searches for a String in a Trie, and returns a map with the result Map.
-- The key of the map is the position of the found element in the IndexItem list
-- and the value is the match score, the higher the worse.
strictTextSearch :: Trie Char (Int, Int) -> String -> Map Int Int
strictTextSearch trie searchTerm =
  mapWithKey (\_ v -> v - (length searchTerm)) (trieTextSearch trie searchTerm)

trieTextSearch :: Trie Char (Int,Int) -> String -> Map Int Int
trieTextSearch (Node _         values) []     = fromList values
trieTextSearch (Node subTries  _)      (t:ts) =
  if member t subTries
    then let Just trie = Data.Map.lookup t subTries
         in trieTextSearch trie ts
    else Data.Map.empty

------------------------------------------------------------------------------
-- Prototypical implementation of fuzzy text search.
-- Produces too many result for short search terms.
-- Thus, it might be reasonable only as an optional search.

--- The maximum amount of letters different between the search term and
--- the result
maxFuzzyDistance :: Int
maxFuzzyDistance = 1

--- The amount of points added to a search result for having a fuzzy mismatch
fuzzyMissmatchScore :: Int
fuzzyMissmatchScore = 10

fuzzyTextSearch :: Ord k => Trie k (Int,Int) -> [k] -> Map Int Int
fuzzyTextSearch t k =
  mapWithKey (\_ v -> v - (length k)) (fuzzyTrieSearch t k maxFuzzyDistance)

fuzzyTrieSearch :: Ord k => Trie k (Int,Int) -> [k] -> Int -> Map Int Int
fuzzyTrieSearch (Node _ v) []     _        = fromList v
fuzzyTrieSearch (Node t v) (k:ks) leftMiss
  | leftMiss == 0 = trieSearch (Node t v) (k:ks)
  | otherwise     = bigUnionWith
                     (elems (mapWithKey (fuzzyTrieSearchrec (k:ks) leftMiss) t))
                     min 
 where
  fuzzyTrieSearchrec :: Ord k => [k] -> Int -> k ->  Trie k (Int, Int) -> Map Int Int
  fuzzyTrieSearchrec []       _  _   _    = Data.Map.empty
  fuzzyTrieSearchrec (k1:ks1) lm key trie
    | key == k1 = fuzzyTrieSearch trie ks1 lm
    | otherwise
    = mapWithKey
        (\_ y -> y + fuzzyMissmatchScore)
        (bigUnionWith
           [fuzzyTrieSearch trie ks1 (lm-1),     -- Both keys get tossed (map and mbp match)
            fuzzyTrieSearch trie (k1:ks1) (lm-1),  -- Only the Trie key gets tossed out (map and mp match)
            fuzzyTrieSearch (Node (fromList [(key,trie)]) []) ks1 (lm-1)]
              -- the search key gets tossed out (mp and map match)
           min )

  bigUnionWith :: Ord a => [Map a b] -> (b -> b -> b) -> Map a b
  bigUnionWith []         _ = Data.Map.empty
  bigUnionWith [x]        _ = x
  bigUnionWith (x1:x2:xs) f = unionWith f x1 (bigUnionWith (x2:xs) f)

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
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
