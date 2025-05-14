-------------------------------------------------------------------------
--- The options of the Currygle indexer and server.
---
--- @author Michael Hanus
--- @version May 2025
-------------------------------------------------------------------------

module Options
  ( Options(..), defaultOptions
  , processOptions, usageText
  , printWhenStatus, printWhenIntermediate, printWhenAll
  )
 where

import Control.Monad         ( when, unless )
import Numeric               ( readNat )
import System.Console.GetOpt

import System.Process        ( exitWith )

import Settings              ( indexDirPath )

-- The options of the Currgle indexer.
data Options = Options
  { optVerb        :: Int     -- verbosity (0: quiet, 1: status,
                              --            2: intermediate, 3: all)
  , optHelp        :: Bool    -- if help info should be printed
  , optDocDir      :: String  -- directory containing .cdoc files
  , optIndex       :: Bool    -- create index?
  , optInteractive :: Bool    -- start interactive search mode?
  , optStartServer :: Bool    -- run in server mode?
  , optStopServer  :: Bool    -- stop the server?
  }

--- The default options of the Currgle indexer.
defaultOptions :: Options
defaultOptions = Options 1 False "" False False False False

--- Process the actual command line arguments and return the options
--- and the name of the main program.
processOptions :: String -> [String] -> IO (Options,[String])
processOptions banner argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultOptions funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> printUsage >> exitWith 1)
  when (optHelp opts) (printUsage >> exitWith 0)
  return (opts, args)
 where
  printUsage = putStrLn (banner ++ "\n" ++ usageText)

-- Help text
usageText :: String
usageText =
  usageInfo ("Usage: currygle [options]\n")
            options

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "v" ["verbosity"]
           (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
           "verbosity level:\n0: quiet (same as `-q')\n1: show status messages (default)\n2: show more details (same as `-v')\n3: show all details"
  , Option "" ["docdir"]
       (ReqArg (\arg opts -> opts { optDocDir = arg }) "<d>")
       "directory containing '.cdoc' files"
  , Option "" ["index"]
           (NoArg (\opts -> opts { optIndex = True }))
           ("generate index into directory '" ++ indexDirPath ++ "'")
  , Option "" ["interactive"]
           (NoArg (\opts -> opts { optInteractive = True }))
           ("start Currygle in interactive search mode")
  , Option "s" ["server"]
           (NoArg (\opts -> opts { optStartServer = True }))
           "start Currygle server mode"
  , Option "" ["stop"]
           (NoArg (\opts -> opts { optStopServer = True }))
           "stop the server"
  ]
 where
  safeReadNat opttrans s opts = case readNat s of
    [(n,"")] -> opttrans n opts
    _        -> error "Illegal number argument (try `-h' for help)"

  checkVerb n opts = if n >= 0 && n <= 3
                       then opts { optVerb = n }
                       else error "Illegal verbosity level (try `-h' for help)"

-------------------------------------------------------------------------

printWhenStatus :: Options -> String -> IO ()
printWhenStatus opts s = when (optVerb opts > 0) (putStrLn s)

printWhenIntermediate :: Options -> String -> IO ()
printWhenIntermediate opts s =
  when (optVerb opts > 1) (putStrLn s)

printWhenAll :: Options -> String -> IO ()
printWhenAll opts s =
 when (optVerb opts > 2) (putStrLn s)

---------------------------------------------------------------------------
