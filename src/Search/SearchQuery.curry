----------------------------------------------------------------------
--- This module provides the data types for search queries in
--- currygle2, and their parsers
---
--- @author Helge Knof
--- @version 07.12.2024
----------------------------------------------------------------------
module Search.SearchQuery (SearchQuery(..), SearchTerm(..),
                            parseSearchText)
  where

import Index.Indexer
import Index.IndexItem
import Index.Signature
import Search.SignatureParser

import FlatCurry.Types
import Data.Char

-- The SearchQuery is a tree which has the SearchTerms as leaves,
-- and AND, OR or NOT AND as the inner nodes
data SearchQuery = Single SearchTerm 
                | AND SearchQuery SearchQuery   
                | OR  SearchQuery SearchQuery
                | NOT SearchQuery SearchQuery
    deriving Show

data SearchTerm = Module String
                | InModule String
                | InPackage String
                | Function String
                | Type String
                | Class String
                | Author String
                | Det
                | NonDet
                | Flexible
                | Rigid
                | Signature (Maybe Signature)
                | All String
    deriving Show

-- Parses a searchtext into a searchquery
parseSearchText :: String -> Maybe SearchQuery
parseSearchText text = case parseSearchQuery text of
                            Nothing -> Nothing
                            Just (sq, _) -> Just sq

-- Gets a String from the user, and turns it into a SearchQuery. It parses until it finds a curly bracket it didn't
-- open, or the String ends
parseSearchQuery :: String -> Maybe (SearchQuery, String)
parseSearchQuery query =  let query1 = dropWhile (\x -> x == ' ') query in
                            if head query1 == '}' then
                              Nothing
                            else if head query1 == '{' then
                              case splitByClosingCurlyBracket (drop 1 query1) 1 of
                                Nothing -> Nothing
                                Just (n, l) ->  if l == "" then
                                                  parseSearchQuery n
                                                else case getNextJunctor l of
                                                  Nothing -> case parseSearchQuery n of
                                                                  Nothing -> Nothing
                                                                  Just (sq1, _) -> case parseSearchQuery l of
                                                                    Nothing -> Nothing
                                                                    Just (sq2, _) -> Just (AND sq1 sq2, "")
                                                  Just (j, jl) -> case parseSearchQuery n of
                                                    Nothing -> Nothing
                                                    Just (sq, _) -> parseJunctor sq (drop jl l) j
                            else
                              let (sq,left) = parseSingle query1 in
                                if dropWhile (\x -> x == ' ') left == "" then
                                  Just (sq, "")
                                else
                                  case getNextJunctor left of
                                    Nothing -> Just (parseSingle left)
                                    Just (j, jl) -> parseJunctor sq (drop jl left) j

parseSingle :: String -> (SearchQuery, String)
parseSingle text = (Single (fst(parseSearchTerm text)), snd(parseSearchTerm text))
-- Takes the leftover string and the first searchQuery which was parsed, and returns an AND searchQuery
-- Assumes the AND is removed, as well as all white spaces
parseJunctor :: SearchQuery -> String -> (SearchQuery -> SearchQuery -> SearchQuery) -> Maybe (SearchQuery, String)
parseJunctor prevSq text junctor =  if dropWhile (\x -> x==' ') text == "" then
                                      Nothing
                                    else
                                      case parseSearchQuery text of
                                          Nothing -> Nothing
                                          Just (sq1, left1) -> Just (junctor prevSq sq1, left1)

-- Gets a String, and returns the next Junctor, AND OR Not, or Nothing if the is none left
getNextJunctor :: String -> Maybe ((SearchQuery->SearchQuery->SearchQuery), Int)
getNextJunctor t = let t1 = dropWhile (\x -> x == ' ') t in
                        if length t1 == 0 then
                          Nothing
                        else if head t1 == '{' then
                          Just (AND, 1)
                        else if take 4 t1 == "AND " then
                          Just (AND, 4)
                        else if take 4 t1 == "NOT " then
                          Just (NOT, 4)
                        else if take 3 t1 == "OR " then
                          Just (OR, 3)
                        else 
                          Just (AND, 0)

--Takes a string to be split, the number of open curly brackets, and returns the split string, if the curly bracket closes
splitByClosingCurlyBracket :: String -> Int -> Maybe (String, String)
splitByClosingCurlyBracket []     _ = Nothing
splitByClosingCurlyBracket (c:cs) n | c == '}' && n >  1 = case splitByClosingCurlyBracket cs (n - 1) of
                                                              Nothing       -> Nothing
                                                              Just (s1,s2)  -> Just (c:s1,s2)
                                    | c == '}' && n == 1 = Just ("", cs)
                                    | c == '{'           = case splitByClosingCurlyBracket cs (n + 1) of
                                                              Nothing       -> Nothing
                                                              Just (s1,s2)  -> Just (c:s1,s2)
                                    | otherwise          = case splitByClosingCurlyBracket cs n of
                                                              Nothing       -> Nothing
                                                              Just (s1,s2)  -> Just (c:s1,s2)
    
-- Parses a SearchTerm. A searchterm ends either, because of the next AND, OR, Not, the end of the string,
-- a : indicating the start of the next searchterm, or an error, because the word after the : is not valid
parseSearchTerm :: String -> (SearchTerm, String)
parseSearchTerm text = let (toBeParsed, leftOver) = getStringUntilEndOfSearchTerm text in
                            if take 7 toBeParsed == ":module" then (Module (adjustTerm (drop 7 toBeParsed)), leftOver) else
                            if take 2 toBeParsed == ":m" then (Module (adjustTerm (drop 2 toBeParsed)), leftOver) else
                            if take 9 toBeParsed == ":inmodule" then (InModule (adjustTerm (drop 9 toBeParsed)), leftOver) else
                            if take 3 toBeParsed == ":im" then (InModule (adjustTerm (drop 3 toBeParsed)), leftOver) else
                            if take 10 toBeParsed == ":inpackage" then (InPackage (adjustTerm (drop 10 toBeParsed)), leftOver) else
                            if take 3 toBeParsed == ":ip" then (InPackage (adjustTerm (drop 3 toBeParsed)), leftOver) else
                            if take 9 toBeParsed == ":function" then (Function (adjustTerm (drop 9 toBeParsed)), leftOver) else
                            if take 2 toBeParsed == ":f" then (Function (adjustTerm (drop 2 toBeParsed)), leftOver) else
                            if take 5 toBeParsed == ":type" then (Type (adjustTerm (drop 5 toBeParsed)), leftOver) else
                            if take 2 toBeParsed == ":t" then (Type (adjustTerm (drop 2 toBeParsed)), leftOver) else
                            if take 6 toBeParsed == ":class" then (Class (adjustTerm (drop 6 toBeParsed)), leftOver) else
                            if take 2 toBeParsed == ":c" then (Class (adjustTerm (drop 2 toBeParsed)), leftOver) else
                            if take 7 toBeParsed == ":author" then (Author (adjustTerm (drop 7 toBeParsed)), leftOver) else
                            if take 2 toBeParsed == ":a" then (Author (adjustTerm (drop 2 toBeParsed)), leftOver) else
                            if take 4 toBeParsed == ":det" then (Det, leftOver) else
                            if take 7 toBeParsed == ":nondet" then (NonDet, leftOver) else
                            if take 3 toBeParsed == ":nd" then (NonDet, leftOver) else
                            if take 9 toBeParsed == ":flexible" then (Flexible, leftOver) else
                            if take 3 toBeParsed == ":fl" then (Flexible, leftOver) else
                            if take 6 toBeParsed == ":rigid" then (Rigid, leftOver) else
                            if take 2 toBeParsed == ":r" then (Rigid, leftOver) else
                            if take 10 toBeParsed == ":signature" then (Signature (parseSignature (drop 10 toBeParsed)), leftOver) else
                            if take 2 toBeParsed == ":s" then (Signature (parseSignature (drop 2 toBeParsed)), leftOver) else
                            if take 4 toBeParsed == ":all" then (All (adjustTerm (drop 4 toBeParsed)), leftOver) else
                            if take 2 toBeParsed == ":a" then (All (adjustTerm (drop 2 toBeParsed)), leftOver) else
                            (All (adjustTerm toBeParsed), leftOver)
    where
        -- Gets all of the String until the next part of the SearchQuery, including the deliminator at the beginning, if one exists.
        -- Also deletes leading white spaces
        getStringUntilEndOfSearchTerm :: String -> (String, String)
        getStringUntilEndOfSearchTerm input = if head input == ':' then
                                                (':' : (fst (parseUntilNextDeliminator (drop 1 input))),
                                                  snd(parseUntilNextDeliminator (drop 1 input)))
                                             else
                                                (fst (parseUntilNextDeliminator input),
                                                 snd (parseUntilNextDeliminator input))
        -- Removes spaces from front and back
        adjustTerm :: String -> String
        adjustTerm str = map toLower (reverse (dropWhile (\x -> x == ' ') (reverse (dropWhile (\x -> x == ' ') str))))

-- Gets a String, goes until the next {, }, :, AND, OR or NOT, and returns the String it walked through, as well as
-- the rest String
parseUntilNextDeliminator :: String -> (String, String)
parseUntilNextDeliminator text = let nextDelPos = getNextDelimPos text in
                                    (take nextDelPos text, drop nextDelPos text)

-- Returns the position of the next deliminator, :, OR, AND or NOT. If none is present, it
-- returns the length of the String
getNextDelimPos :: String -> Int
getNextDelimPos []     = 0
getNextDelimPos (x:xs) = if x == ':' then 0 else
                         if x == '{' then 0 else
                         if x == '}' then 0 else
                         if take 4 (x:xs) == "AND " then 0 else
                         if take 3 (x:xs) == "OR " then 0 else
                         if take 4 (x:xs) == "NOT " then 0 else
                         1 + getNextDelimPos xs
