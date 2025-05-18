-----------------------------------------------------------------------------
--- This module provides the operation `searchInteractive` which implements
--- a simple search interface for Currygle in the console.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
-----------------------------------------------------------------------------

module Interactive ( searchInteractive )
  where

import Data.Time

import Index.Indexer
import Index.IndexItem
import Search.SearchQuery
import Search.Search

-- Execute interactive search w.r.t. a directory containing the index.
searchInteractive :: FilePath -> IO ()
searchInteractive indexFolder =
    do  index <- readIndex indexFolder
        mainLoop index

mainLoop :: Index -> IO ()
mainLoop index = do
  putStrLn "Please enter Currygle query:"
  searchText <- getLine
  case parseSearchText searchText of
    Nothing -> putStrLn "Syntax error"
    Just sq -> do (items,etime) <- profilingCurrygleSearch index sq
                  putStr $ unlines $ line : map prettyResult items ++ [line]
                  putStrLn $ show (length items) ++ " results" ++
                             " (found in " ++ show etime ++ " msec)"
  mainLoop index
 where
  line = take 60 (repeat '-')

prettyResult :: (IndexItem,Int) -> String
prettyResult (TypeItem (TypeIndex name mn pkg _ _ _ _),score) =
  "Type " ++ name ++ " (module " ++ mn ++ ", package " ++ pkg ++ eScore score
prettyResult (FunctionItem (FunctionIndex name mn pkg _ _ _ _ _),score) =
  name ++ " (module " ++ mn ++ ", package " ++ pkg ++ eScore score
prettyResult (ModuleItem (ModuleIndex name _ pkg _ _), score) =
  "Module " ++ name ++ " (package " ++ pkg ++ eScore score

eScore :: Int -> String
eScore score = ", score " ++ show score ++ ")"

-----------------------------------------------------------------------------
