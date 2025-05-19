----------------------------------------------------------------------
--- This module provides the data types for search queries in Currygle
--- together with parsers and pretty printers.
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module Search.Query
  ( SearchQuery(..), SearchTerm(..), prettySearchQuery, parseSearchText )
 where

import Data.Char
import Data.List              ( isPrefixOf, scanl )
import FlatCurry.Types

import Index.Helper           ( strip )
import Index.Indexer
import Index.IndexItem
import Index.Signature
import Search.SignatureParser

-- The SearchQuery is a tree which has the SearchTerms as leaves,
-- and AND, OR or NOT AND as the inner nodes
data SearchQuery = Single SearchTerm 
                 | AND SearchQuery SearchQuery   
                 | OR  SearchQuery SearchQuery
                 | NOT SearchQuery SearchQuery

prettySearchQuery :: SearchQuery -> String
prettySearchQuery q = prettySQ False q
 where
  prettySQ _  (Single st)   = prettySearchTerm st
  prettySQ br (AND st1 st2) = prettySQ2 br "AND" st1 st2
  prettySQ br (OR  st1 st2) = prettySQ2 br "OR"  st1 st2
  prettySQ br (NOT st1 st2) = prettySQ2 br "NOT" st1 st2

  prettySQ2 br op st1 st2 =
    bracketIf br (prettySQ True st1 ++ " " ++ op ++ " " ++ prettySQ True st2)

  bracketIf wb s = if wb then "{" ++ s ++ "}" else s

data SearchTerm = Description String
                | Module String
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

prettySearchTerm :: SearchTerm -> String
prettySearchTerm (Description s) = ":description " ++ s -- should not occur
prettySearchTerm (Module      s) = ":module " ++ s
prettySearchTerm (InModule    s) = ":inmodule " ++ s
prettySearchTerm (InPackage   s) = ":inpackage " ++ s
prettySearchTerm (Function    s) = ":function " ++ s
prettySearchTerm (Type        s) = ":type " ++ s
prettySearchTerm (Class       s) = ":class " ++ s
prettySearchTerm (Author      s) = ":author " ++ s
prettySearchTerm (Signature   s) = ":signature " ++ maybe "" (prettySig False) s
prettySearchTerm Det             = ":deterministic"
prettySearchTerm NonDet          = ":nondeterministic"
prettySearchTerm Flexible        = ":flexible"
prettySearchTerm Rigid           = ":rigid"
prettySearchTerm (All         s) = s

-- Parses a searchtext into a searchquery
parseSearchText :: String -> Maybe SearchQuery
parseSearchText text = case parseSearchQuery text of
  Nothing      -> Nothing
  Just (sq, _) -> Just sq

-- Transforms a query string into a SearchQuery.
-- It parses until it finds a curly bracket it didn't open, or the String ends
parseSearchQuery :: String -> Maybe (SearchQuery, String)
parseSearchQuery query = 
  let query1 = dropWhile (\x -> x == ' ') query in
  if head query1 == '}' then
    Nothing
  else if head query1 == '{' then
    case splitByClosingCurlyBracket (drop 1 query1) 1 of
      Nothing -> Nothing
      Just (n, l) ->  if null l then
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
    
-- Parses a SearchTerm. A SearchTerm ends either, because of the next AND, OR, Not, the end of the string,
-- a : indicating the start of the next searchterm, or an error, because the word after the : is not valid
parseSearchTerm :: String -> (SearchTerm, String)
parseSearchTerm text =
  let (toBeParsed, leftOver) = getStringUntilEndOfSearchTerm text
      (wd,wds) = break (==' ') toBeParsed
  in if take 1 wd == ":"
       then case completeOption (tail wd) of
              "module"           -> (Module (adjustTerm wds), leftOver)
              "inmodule"         -> (InModule (adjustTerm wds), leftOver)
              "inpackage"        -> (InPackage (adjustTerm wds), leftOver)
              "function"         -> (Function (adjustTerm wds), leftOver)
              "type"             -> (Type (adjustTerm wds), leftOver)
              "class"            -> (Class (adjustTerm wds), leftOver)
              "author"           -> (Author (adjustTerm wds), leftOver)
              "signature"        -> (Signature (parseSignature wds), leftOver)
              "deterministic"    -> (Det, leftOver)
              "nondeterministic" -> (NonDet, leftOver)
              "flexible"         -> (Flexible, leftOver)
              "rigid"            -> (Rigid, leftOver)
              _                  -> (All (adjustTerm toBeParsed), leftOver)
       else (All (adjustTerm toBeParsed), leftOver)
 where
  -- complete abbreviated options (take the first if not unique)
  completeOption s = case filter (s `isPrefixOf`) options of (o:_) -> o
                                                             _     -> ""

  options = [ "module", "function", "class", "type", "inmodule", "inpackage"
            , "author", "signature", "deterministic", "nondeterministic"
            , "flexible", "rigid" ]

  -- Gets all of the String until the next part of the SearchQuery,
  -- including the deliminator at the beginning, if one exists.
  -- Also deletes leading white spaces
  getStringUntilEndOfSearchTerm :: String -> (String, String)
  getStringUntilEndOfSearchTerm input
    | head input == ':'
    = (':' : (fst (parseUntilNextDeliminator (drop 1 input))),
       snd(parseUntilNextDeliminator (drop 1 input)))
    | otherwise
    = (fst (parseUntilNextDeliminator input),
       snd (parseUntilNextDeliminator input))

  -- Removes spaces from front and back
  adjustTerm :: String -> String
  adjustTerm = map toLower . strip

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
