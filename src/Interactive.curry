----------------------------------------------------------------------
--- This module provides the runSearchLocally function, which runs a
--- currygle2 search interface in the console
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------
module Interactive ( runSearchLocally )
  where

import Data.Time

import Index.Indexer
import Index.IndexItem
import Search.SearchQuery
import Search.Search

-- Gets the path to a remporary folder with a prepared index, and runs the search on it
runSearchLocally :: FilePath -> IO ()
runSearchLocally indexFolder =
    do  index <- readIndex indexFolder
        mainLoop index

mainLoop :: Index -> IO ()
mainLoop index = do
  putStrLn "Please enter searchquery"
  searchText <- getLine
  time <- case parseSearchText searchText of
            Nothing -> putStrLn "Syntax error" >> return 0
            Just sq -> measureTime (\x -> length (currygle2search index x)) sq
  putStrLn $ show time ++ " msec"
  mainLoop index

prettyResult :: [IndexItem] -> String
prettyResult [] = "---------------------------------"
prettyResult (TypeItem (TypeIndex name _ _ _ _ _ _):xs) = "----------------------\n"
                                                                    ++ name ++ "\n"
                                                                    ++ prettyResult xs
prettyResult (FunctionItem (FunctionIndex name _ _ _ _ _ _ _):xs) = "----------------------\n"
                                                                    ++ name ++ "\n"
                                                                    ++ prettyResult xs
prettyResult (ModuleItem (ModuleIndex name _ _ _ _):xs) = "----------------------\n"
                                                                    ++ name ++ "\n"
                                                                    ++ prettyResult xs
                                                                
-- Measures the time a function f needs to evaluate with x as the argument, prints the result of the function
-- and returns the time in seconds
measureTime :: Show b => ( a -> b ) -> a -> IO Int
measureTime f x = do
  startClockTime <- getClockTime
  startTime <- let a = clockTimeToInt startClockTime in return a
  putStrLn $ show (f x) ++ " results"
  endClockTime <- getClockTime
  endTime <- return (clockTimeToInt endClockTime)
  return (endTime - startTime)