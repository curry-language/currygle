-----------------------------------------------------------------------------
--- This module provides the operation `searchInteractive` which implements
--- a simple search interface for Currygle in the console.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
-----------------------------------------------------------------------------

module Interactive ( searchInteractive )
  where

import Index.Indexer
import Index.Item
import Options
import Search.Query
import Search.Execute

-- Execute interactive search w.r.t. a directory containing the index.
searchInteractive :: Options -> FilePath -> IO ()
searchInteractive opts indexFolder = do
  printWhenStatus opts "Please wait for reading Currygle index files..."
  readIndexStrict opts indexFolder >>= mainLoop

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
prettyResult (TypeItem name mn pkg _ _ _, score) =
  "Type " ++ name ++ " (module " ++ mn ++ ", package " ++ pkg ++ eScore score
prettyResult (FunctionItem name mn pkg _ _ _ _, score) =
  name ++ " (module " ++ mn ++ ", package " ++ pkg ++ eScore score
prettyResult (ModuleItem name _ pkg _, score) =
  "Module " ++ name ++ " (package " ++ pkg ++ eScore score

eScore :: Int -> String
eScore score = ", score " ++ show score ++ ")"

-----------------------------------------------------------------------------
