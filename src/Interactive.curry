-----------------------------------------------------------------------------
--- This module provides the operation `searchInteractive` which implements
--- a simple search interface for Currygle in the console.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
-----------------------------------------------------------------------------

module Interactive ( searchInteractive )
  where

import Control.Monad  ( unless )
import Data.List      ( isPrefixOf )
import System.IO      ( hIsEOF, stdin )

import Index.Helper   ( strip )
import Index.Indexer
import Index.Item
import Options
import Search.Query   ( parseSearchText )
import Search.Execute ( profilingCurrygleSearch )

-- Execute interactive search w.r.t. a directory containing the index.
searchInteractive :: Options -> FilePath -> IO ()
searchInteractive opts indexFolder = do
  printWhenStatus opts "Please wait for reading Currygle index files..."
  readIndexStrict opts indexFolder >>= mainLoop

mainLoop :: Index -> IO ()
mainLoop index = do
  putStrLn "Please enter Currygle query (with prefix ':fuzzy' or ':halt'):"
  iseof <- hIsEOF stdin
  if iseof
    then mainLoop index
    else do input <- strip <$> getLine
            if input `elem` [":halt", ":h"]
              then putStrLn "Bye!"
              else processSearchQuery input >> mainLoop index
 where
  processSearchQuery searchtext = do
    let (fuzzy,query) = if ":fuzzy" `isPrefixOf` searchtext
                          then (True, strip (drop 6 searchtext))
                          else (False,searchtext)
    
    unless (null query) $ case parseSearchText query of
      Nothing -> putStrLn "Syntax error"
      Just sq -> do (items,etime) <- profilingCurrygleSearch fuzzy index sq
                    putStr $ unlines $ line : map prettyResult items ++ [line]
                    putStrLn $ show (length items) ++ " results" ++
                               " (found in " ++ show etime ++ " msec)"
   where
    line = take 60 (repeat '-')

prettyResult :: (IndexItem,Int) -> String
prettyResult (TypeItem name mn pkg age _ _ _, score) =
  "Type " ++ name ++ " (module " ++ mn ++ ", package " ++ pkg ++ eScore age score
prettyResult (FunctionItem name mn pkg age _ _ _ _, score) =
  name ++ " (module " ++ mn ++ ", package " ++ pkg ++ eScore age score
prettyResult (ModuleItem name _ pkg age _, score) =
  "Module " ++ name ++ " (package " ++ pkg ++ eScore age score

eScore :: Int -> Int -> String
eScore _ score = -- ", age " ++ show age ++
                 ", score " ++ show score ++ ")"

-----------------------------------------------------------------------------
