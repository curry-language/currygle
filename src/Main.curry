----------------------------------------------------------------------
--- The main module which combines the indexer and the search server.
---
--- @author Michael Hanus
--- @version May 2025
----------------------------------------------------------------------

module Main ( main ) where

import Control.Monad      ( unless, when )
import System.Environment ( getArgs )

import System.Process     ( exitWith )

import Interactive   ( searchInteractive )
import Index.Indexer ( createIndexFromDir, writeIndex )
import Options
import Server        ( startServer, stopServer )
import Settings      ( indexDirPath, currygleDate )

main :: IO ()
main = do
  (opts,args) <- getArgs >>= processOptions
  unless (null args) $ do
    putStrLn $ "Superfluous arguments: " ++ unwords args
    exitWith 1
  let indexdir = indexDirPath
  when (optIndex opts) $ do
    let docdir = optDocDir opts
    when (null docdir) $ putStrLn "Option '--docdir' missing!" >> exitWith 1
    printWhenStatus opts $ "Creating index from files in '" ++ docdir ++ "'..."
    index <- createIndexFromDir docdir
    writeIndex opts indexdir index
    printWhenStatus opts $ "Index created and stored in '" ++ indexdir ++ "'."
    exitWith 0
  when (optStartServer opts) $ startServer opts >> exitWith 0
  when (optStopServer opts)  $ stopServer  opts >> exitWith 0
  when (optInteractive opts) $ do
    printWhenStatus opts "Running query search in interactive mode..."
    searchInteractive opts indexdir
    exitWith 0
  printUsage
