------------------------------------------------------------------------------
-- | Author : Michael Hanus
--   Version: September 2025
--
-- This module defines auxiliary operations to support the crawler,
-- e.g., to retrieve information about deprecated packages from Masala.
------------------------------------------------------------------------------
{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module Crawler.Helper where

import Control.Monad  ( when )

import System.Process ( system )
import Text.CSV       ( readCSVFile )

------------------------------------------------------------------------------
-- Retrieve a list of package versions with their deprecated status
-- by querying Masala.
downloadPackageVersions :: IO [(String,String,Bool)]
downloadPackageVersions = do
  let csvf = "MASALA.csv"
  ec <- system $ curlCommand ++ " -o '" ++ csvf ++ "' '" ++ downloadURL ++ "'"
  if ec > 0
    then do putStrLn "Cannot download deprecation info from Masala!"
            return []
    else catch (fmap (map mapCol) (readCSVFile csvf) >>= \v -> return $!! v)
               (\_ -> do putStrLn "Cannot parse deprecation info from Masala!"
                         return [])
 where
  mapCol col = (col!!0, col!!1, read (col!!3) :: Bool)

curlCommand :: String
curlCommand = unwords ["curl","--max-time","20","--silent","--show-error"]

downloadURL :: String
downloadURL = "https://cpm.curry-lang.org/masala/run.cgi?versions-csv"

------------------------------------------------------------------------------
