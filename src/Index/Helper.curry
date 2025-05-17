-----------------------------------------------------------------------
--- This module defines a `ReadWrite` instance of type `Map`.
---
--- @author Michael Hanus
--- @version May 2025
-----------------------------------------------------------------------
{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module Index.Helper where

import System.IO  ( hPutChar )

import Data.Map
import RW.Base

-- Strips leading and trailing spaces from a string.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

------------------------------------------------------------------------------
--- `ReadWrite` instance of `Map`.
instance (ReadWrite a,ReadWrite b) => ReadWrite (Map a b) where
  readRW _    ('0' : r0) = (Tip,r0)
  readRW strs ('1' : r0) = (Bin a' b' c' d' e',r5)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1
      (c',r3) = readRW strs r2
      (d',r4) = readRW strs r3
      (e',r5) = readRW strs r4

  showRW _      strs0 Tip = (strs0,showChar '0')
  showRW params strs0 (Bin a' b' c' d' e') =
    (strs5,showChar '1' . (show1 . (show2 . (show3 . (show4 . show5)))))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'
      (strs3,show3) = showRW params strs2 c'
      (strs4,show4) = showRW params strs3 d'
      (strs5,show5) = showRW params strs4 e'

  writeRW _      h Tip strs = hPutChar h '0' >> return strs
  writeRW params h (Bin a' b' c' d' e') strs =
    hPutChar h '1'
     >> ((((writeRW params h a' strs >>= writeRW params h b')
            >>= writeRW params h c')
           >>= writeRW params h d')
          >>= writeRW params h e')

  typeOf n = RWType "Map" [typeOf (get_a n),typeOf (get_b n)]
    where
      get_a :: Map a' b' -> a'
      get_a _ = failed
      get_b :: Map a' b' -> b'
      get_b _ = failed

------------------------------------------------------------------------------
