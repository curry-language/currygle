----------------------------------------------------------------------
--- This module implements a search server for Currygle.
---
--- @author Michael Hanus
--- @version May 2025
----------------------------------------------------------------------

module Server
  ( startServer, stopServer, pingServer, searchClient, profilingSearchClient )
 where

import Data.List          ( isPrefixOf )
import System.IO

import Network.Socket
import System.Process     ( sleep )

import Index.Helper       ( strip )
import Index.Indexer
import Index.Item
import Options
import Search.Query
import Search.Execute
import Settings           ( indexDirPath, serverSocket )

------------------------------------------------------------------------------
-- Start the search server on a socket:
startServer :: Options -> IO ()
startServer opts = do
  let snr = serverSocket
  printWhenStatus opts $ "Starting server on socket " ++ show snr
  socket <- listenOn snr
  printWhenStatus opts $ "Server listening on socket " ++ show snr
  readIndexStrict opts indexDirPath >>= serverLoop opts socket

-- This action is the main server loop.
serverLoop :: Options -> Socket -> Index -> IO ()
serverLoop opts socket index = do
  printWhenIntermediate opts $ "Ready to accept connection"
  (client, handle) <- accept socket
  printWhenStatus opts $ "Connection from client: " ++ client
  if client == localhost || (localhost ++ ":") `isPrefixOf` client
    then serverLoopOnHandle opts socket index handle
    else do
      printMessage $ "Connection from illegal client: " ++ client
      hClose handle
      serverLoop opts socket index
 where
  localhost = "127.0.0.1"

serverLoopOnHandle :: Options -> Socket -> Index -> Handle -> IO ()
serverLoopOnHandle opts socket index handle = do
  eof <- hIsEOF handle
  if eof
    then do
      hClose handle
      printWhenAll opts "SERVER connection: eof"
      serverLoop opts socket index
    else do
      request <- strip <$> hGetLineUntilEOF handle
      printWhenStatus opts $ "MESSAGE RECEIVED: '" ++ request ++ "'"
      hFlush stdout
      case break (==' ') request of
        ("STOP","") -> hClose handle >> printWhenStatus opts "Server stopped"
        ("PING","") -> sendResult "ALIVE!"
        ("QUERY", ' ':q) -> searchSend (\i -> return . currygleSearch False i) q
        ("FQUERY",' ':q) -> searchSend (\i -> return . currygleSearch True i) q
        ("TIME",  ' ':q) -> searchSend (profilingCurrygleSearch False) q
        ("FTIME", ' ':q) -> searchSend (profilingCurrygleSearch True) q
        _ -> printMessage $ "ILLEGAL MESSAGE RECEIVED: " ++ request
 where
  searchSend searchop query = do
    answer <- case parseSearchText query of
                 Nothing -> return "Parse error"
                 Just q  -> show <$> searchop index q
    sendResult answer

  sendResult resultstring = do
    printWhenAll opts $ "Result:\n" ++ resultstring
    hPutStrLn handle resultstring
    hFlush handle
    serverLoopOnHandle opts socket index handle

-- This action reads from a handle until it reaches EOL or EOF.
hGetLineUntilEOF  :: Handle -> IO String
hGetLineUntilEOF h = do
  eof <- hIsEOF h
  if eof
   then return ""
   else do c <- hGetChar h
           if c=='\n'
             then return ""
             else do cs <- hGetLineUntilEOF h
                     return (c:cs)

------------------------------------------------------------------------------
-- Stop the server:
stopServer :: Options -> IO ()
stopServer _ = do
  h <- connectToSocket "localhost" serverSocket
  hPutStrLn h "STOP"
  hFlush h
  hClose h

-- Ping the server. Returns `True` if the server is reachable.
pingServer :: IO Bool
pingServer = catch ping (\_ -> return False)
 where
  ping = do
    h <- connectToSocket "localhost" serverSocket
    hPutStrLn h "PING"
    hGetLine h
    hFlush h
    hClose h
    return True

--- Currygle search by using the server.
--- Returns the string representation of the results.
--- If the first argument is `True`, fuzzy search is used.
searchClient :: Bool -> String -> IO String
searchClient fuzzy request = do
  h <- connectToSocket "localhost" serverSocket
  hPutStrLn h ((if fuzzy then "FQUERY " else "QUERY ") ++ request)
  hFlush h
  answer <- hGetLine h
  --putStrLn $ "Answer: " ++ answer
  hClose h
  return answer

--- Currygle search with profiling by using the server. Return the string
--- representation of the pair of items and the elapsed time.
--- If the first argument is `True`, fuzzy search is used.
profilingSearchClient :: Bool -> String -> IO String
profilingSearchClient fuzzy request = do
  h <- connectToSocket "localhost" serverSocket
  hPutStrLn h ((if fuzzy then "FTIME " else "TIME ") ++ request)
  hFlush h
  answer <- hGetLine h
  --putStrLn $ "Answer: " ++ answer
  hClose h
  return answer

------------------------------------------------------------------------------
