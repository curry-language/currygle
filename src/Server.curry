----------------------------------------------------------------------
--- This module implements a search server for Currygle.
---
--- @author Michael Hanus
--- @version May 2025
----------------------------------------------------------------------

module Server ( startServer, stopServer, pingServer, searchClient )
  where

import Network.Socket
import System.IO
import System.Process     ( sleep )

import Index.Indexer
import Index.IndexItem
import Options
import Search.SearchQuery
import Search.Search
import Settings           ( indexDirPath, serverSocket )

------------------------------------------------------------------------------
-- Start the search server on a socket:
startServer :: Options -> IO ()
startServer opts = do
  let snr = serverSocket
  socket <- listenOn snr
  printWhenStatus opts $ "Server listening on socket " ++ show snr
  index <- readIndex indexDirPath
  serverLoop opts socket $!! index

-- This action is the main server loop.
serverLoop :: Options -> Socket -> Index -> IO ()
serverLoop opts socket index = do
  connection <- waitForSocketAccept socket (-1)
  case connection of
    Just (_, handle) -> serverLoopOnHandle opts socket index handle
    Nothing -> do
      printWhenStatus opts "serverLoop: time out in waitForSocketAccept"
      sleep 1
      serverLoop opts socket index

serverLoopOnHandle :: Options -> Socket -> Index -> Handle -> IO ()
serverLoopOnHandle opts socket index handle = do
  eof <- hIsEOF handle
  if eof
    then do
      hClose handle
      printWhenAll opts "SERVER connection: eof"
      serverLoop opts socket index
    else do
      request <- fmap strip $ hGetLineUntilEOF handle
      printWhenStatus opts $ "MESSAGE RECEIVED: '" ++ request ++ "'"
      hFlush stdout
      if request == stopMessage
        then
          hClose handle >> printWhenStatus opts "Server stopped"
        else
          if request == pingMessage
            then sendResult "ALIVE!"
            else do let answer = case parseSearchText request of
                                   Nothing -> "Parse error"
                                   Just sq -> show (currygle2search index sq)
                    sendResult answer
 where
  sendResult resultstring = do
    printWhenAll opts $ "Result:\n" ++ resultstring
    hPutStrLn handle resultstring
    hFlush handle
    serverLoopOnHandle opts socket index handle
  
  strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- This action reads from a handle until it reaches EOF.
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
  hPutStrLn h stopMessage
  hFlush h
  hClose h

-- Stop message for the server. Since this is an illegal search syntax,
-- it will never be sent from the web search page.
stopMessage :: String
stopMessage = "{STOP!"

-- Ping the server. Returns `True` if the server is reachable.
pingServer :: IO Bool
pingServer = catch ping (\_ -> return False)
 where
  ping = do
    h <- connectToSocket "localhost" serverSocket
    hPutStrLn h pingMessage
    hGetLine h
    hFlush h
    hClose h
    return True

-- Ping message for the server. Since this is an illegal search syntax,
-- it will never be sent from the web search page.
pingMessage :: String
pingMessage = "{PING!"

-- Currygle search by using the server:
searchClient :: String -> IO String
searchClient request = do
  h <- connectToSocket "localhost" serverSocket
  hPutStrLn h request
  hFlush h
  answer <- hGetLine h
  --putStrLn $ "Answer: " ++ answer
  hClose h
  return answer

------------------------------------------------------------------------------
