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
import Index.Item
import Index.Signature
import Search.SignatureParser ( parseSignature )

--- A `SearchQuery` is a tree which has `SearchTerm`s as leaves,
--- and AND, OR, or NOT as the inner nodes.
data SearchQuery = Single SearchTerm 
                 | AND SearchQuery SearchQuery   
                 | OR  SearchQuery SearchQuery
                 | NOT SearchQuery SearchQuery

--- Pretty prints a `SearchQuery`.
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

--- A `SearchTerm` is either a search option string or just a string (`All`).
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

--- Pretty prints a `SearchQuery`.
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

------------------------------------------------------------------------------
-- Parses a string into a `SearchQuery`.
parseSearchText :: String -> Maybe SearchQuery
parseSearchText text = case parseSearchQuery text of
  Nothing      -> Nothing
  Just (sq, _) -> Just sq

-- Transforms a query string into a SearchQuery.
-- It parses until it finds a curly bracket it didn't open, or the String ends
parseSearchQuery :: String -> Maybe (SearchQuery, String)
parseSearchQuery querystring = 
  case dropWhile (== ' ') querystring of
    '}':_  -> Nothing
    '{':q2 -> do (n,l) <- splitByClosingCurlyBracket q2 1
                 if null l
                  then parseSearchQuery n
                  else case getNextJunctor l of
                         Nothing -> do (sq1,_) <- parseSearchQuery n
                                       (sq2,_) <- parseSearchQuery l
                                       return (AND sq1 sq2, "")
                         Just (j, jl) ->  do (sq,_) <- parseSearchQuery n
                                             parseJunctor sq (drop jl l) j
    q1     ->  let (sq,left) = parseSingle q1
               in if dropWhile (\x -> x == ' ') left == ""
                    then Just (sq, "")
                    else case getNextJunctor left of
                           Nothing      -> Just (parseSingle left)
                           Just (j, jl) -> parseJunctor sq (drop jl left) j

parseSingle :: String -> (SearchQuery, String)
parseSingle text = (Single searchterm, rems)
 where (searchterm,rems) = parseSearchTerm text

-- Takes the leftover string and the first searchQuery which was parsed,
-- and returns an AND searchQuery.
-- Assumes the AND is removed, as well as all white spaces
parseJunctor :: SearchQuery -> String
             -> (SearchQuery -> SearchQuery -> SearchQuery)
             -> Maybe (SearchQuery, String)
parseJunctor prevSq text junctor =
  if null (dropWhile (==' ') text)
    then Nothing
    else do (sq1, left1) <- parseSearchQuery text
            return (junctor prevSq sq1, left1)

-- Gets a String, and returns the next Junctor, AND OR Not, or Nothing
-- if the is none left
getNextJunctor :: String -> Maybe ((SearchQuery->SearchQuery->SearchQuery), Int)
getNextJunctor t
  | length t1 == 0      = Nothing
  | head t1 == '{'      = Just (AND, 1)
  | take 4 t1 == "AND " =  Just (AND, 4)
  | take 4 t1 == "NOT " =  Just (NOT, 4)
  | take 3 t1 == "OR "  =  Just (OR, 3)
  | otherwise           =  Just (AND, 0)
 where
  t1 = dropWhile (== ' ') t

-- Takes a string to be split, the number of open curly brackets,
-- and returns the split string, if the curly bracket closes.
splitByClosingCurlyBracket :: String -> Int -> Maybe (String, String)
splitByClosingCurlyBracket []     _ = Nothing
splitByClosingCurlyBracket (c:cs) n
  | c == '}' && n >  1 = do (s1,s2) <- splitByClosingCurlyBracket cs (n - 1)
                            return (c:s1,s2)
  | c == '}' && n == 1 = return ("", cs)
  | c == '{'           = do (s1,s2) <- splitByClosingCurlyBracket cs (n + 1)
                            return (c:s1,s2)
  | otherwise          = do (s1,s2) <- splitByClosingCurlyBracket cs n
                            return (c:s1,s2)
    
-- Parses a string into a `SearchTerm` which is returned together with the
-- unparsed remaining string.
-- A `SearchTerm` ends either, because of the next AND, OR, NOT,
-- the end of the string, a `:` indicating the start of the next `SearchTerm`,
-- or an error, because the word after the `:` is not valid.
parseSearchTerm :: String -> (SearchTerm, String)
parseSearchTerm text =
  let (toBeParsed, leftOver) = getStringUntilEndOfSearchTerm text
      (wd,wds) = break (==' ') toBeParsed
  in case wd of
       (':':opt) ->
          case completeOption opt of
            "module"           -> (Module (stripToLower wds), leftOver)
            "inmodule"         -> (InModule (stripToLower wds), leftOver)
            "inpackage"        -> (InPackage (stripToLower wds), leftOver)
            "function"         -> (Function (stripToLower wds), leftOver)
            "type"             -> (Type (stripToLower wds), leftOver)
            "class"            -> (Class (stripToLower wds), leftOver)
            "author"           -> (Author (stripToLower wds), leftOver)
            "signature"        -> (Signature (parseSignature wds), leftOver)
            "deterministic"    -> (Det, leftOver)
            "nondeterministic" -> (NonDet, leftOver)
            "flexible"         -> (Flexible, leftOver)
            "rigid"            -> (Rigid, leftOver)
            _                  -> (All (stripToLower toBeParsed), leftOver)
       _ -> (All (stripToLower toBeParsed), leftOver)
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

  -- Removes spaces from front and back and maps the string to lowercase.
  stripToLower :: String -> String
  stripToLower = map toLower . strip

-- Gets a String, goes until the next {, }, :, AND, OR or NOT, and returns
-- the String it walked through, as well as the rest String.
parseUntilNextDeliminator :: String -> (String, String)
parseUntilNextDeliminator text =
  let nextDelPos = getNextDelimPos text
  in (take nextDelPos text, drop nextDelPos text)

-- Returns the position of the next deliminator, :, OR, AND or NOT.
-- If none is present, it returns the length of the String.
getNextDelimPos :: String -> Int
getNextDelimPos []     = 0
getNextDelimPos (x:xs) =
  if x == ':' || x == '{' || x == '}' ||
    take 4 (x:xs) == "AND " || take 3 (x:xs) == "OR " || take 4 (x:xs) == "NOT "
    then 0
    else 1 + getNextDelimPos xs
