----------------------------------------------------------------------
--- The main module which combines the indexer and the search server.
---
--- @author Michael Hanus
--- @version May 2025
----------------------------------------------------------------------

module Main where

import Control.Monad      ( unless, when )
import System.Environment ( getArgs )

import System.Process     ( exitWith )

import Interactive   ( runSearchLocally )
import Index.Indexer ( createIndexFromDir, writeIndex )
import Options
import PackageConfig ( packageVersion )
import Server        ( startServer, stopServer )
import Settings      ( indexDirPath )

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Currygle (Version " ++ packageVersion ++ " of 15/05/25)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  (opts,args) <- getArgs >>= processOptions banner
  unless (null args) $ do
    putStrLn $ "Superfluous arguments: " ++ unwords args
    exitWith 1
  let indexdir = indexDirPath
  when (optIndex opts) $ do
    let docdir = optDocDir opts
    when (null docdir) $ putStrLn "Option '--docdir' missing!" >> exitWith 1
    printWhenStatus opts $ "Creating index from files in '" ++ docdir ++ "'..."
    index <- createIndexFromDir docdir
    writeIndex index indexdir
    printWhenStatus opts $ "Index created and stored in '" ++ indexdir ++ "'."
    exitWith 0
  when (optStartServer opts) $ startServer opts >> exitWith 0
  when (optStopServer opts)  $ stopServer  opts >> exitWith 0
  when (optInteractive opts) $ do
    printWhenStatus opts "Running query search in interactive mode:"
    runSearchLocally indexdir
    exitWith 0
  putStrLn "No execution mode specified. Use '--help' for usage information."
