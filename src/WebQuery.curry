----------------------------------------------------------------------
--- This module handles writing the html pages for currygle
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module WebQuery
    where

import Data.List      ( intersperse )
import System.IO

import HTML.Base
import HTML.Styles.Bootstrap4 ( ehrefDarkBadge, kbdInput )
import Network.URL     ( string2urlencoded, urlencoded2string )
import System.FilePath ( (</>) )
import System.Process  ( system )

import Index.IndexItem
import Index.Indexer
import Index.Signature
import PackageConfig      ( packageVersion )
import Settings
import Search.Search
import Search.SearchQuery
import Server             ( profilingSearchClient, searchClient )

main :: IO HtmlPage
main = do
  uparam <- fmap urlencoded2string getUrlParameter
  if null uparam then return defaultPage
                 else do let showall = take 4 uparam == ":all"
                             query   = if showall then drop 4 uparam else uparam
                         getResultPage True showall query

-- Default page with Currygle description.
defaultPage :: HtmlPage
defaultPage = curryglePage currygleDescription

-- Gets an HtmlRef to a textfield, an HtmlEnv, and creates an HtmlPage from it,
-- showing the search result for the String in the HtmlEnv under HtmlRef
getResultPage :: Bool -> Bool -> String -> IO HtmlPage
getResultPage withserver showall searchtxt
  | all isSpace searchtxt -- no query terms present
  = return defaultPage
  | otherwise
  = case parseSearchText searchtxt of
      Nothing  -> return defaultPage
      Just query ->
        if withserver
          then do
            mbres <- catch (profilingSearchClient searchtxt >>= return . Just)
                           (\_ -> return Nothing)
            case mbres of
              Nothing  -> do -- try to restart server:
                             system "make restart >> SERVER.LOG"
                             resultFromIndex query
              Just res -> case reads res of
                            [((items,et),_)] -> return $ resultPage query "" items et
                            _                -> return defaultPage
          else resultFromIndex query
 where
  resultFromIndex query = do
    index <- readIndex indexDirPath
    return $ resultPage query " (slow search since server not reachable)"
                        (currygleSearch index query) 0

  resultPage query note items etime = curryglePage $
    [ par $
        [ italic [htxt $ "Search results for "]
        , kbdInput [htxt (prettySearchQuery query)], htxt $ note ++ ": "
        , htxt $ "found "
        , htxt $ case num of 0 -> "no entity"
                             1 -> "one entity"
                             _ -> show num ++ " entities"
        , htxt $ if etime==0 then "" else " in " ++ show etime ++ " ms" ] ++
        (if num <= maxresults
           then []
           else [ htxt $ " (showing " ++ show (min num maxresults) ++
                         " results, "
                , href ('?' : string2urlencoded (":all" ++ searchtxt))
                [htxt "show all"], htxt ")"])
    , searchResults $ take maxresults items]
   where
    num        = length items
    maxresults = if showall then num else maxSearchResults

--- The search form consisting of a field and search button.
searchForm :: HtmlFormDef ()
searchForm = simpleFormDefWithID "WebQuery.searchForm"
  [ textField ref "" `addAttrs`
      [("class", "form-control mr-sm-2 flex-fill"),
       ("placeholder","Search entities in Curry packages")],
    addAttr (button "Search!" (\env -> getResultPage True False (env ref)))
            ("class", "btn btn-outline-light my-2 my-sm-0")
  ]
 where ref free

-- Format search results.
searchResults :: [IndexItem] -> BaseHtml
searchResults items =
  table (map itemToHtml items) `addClass` "table table-sm table-hover"
 where
  itemToHtml :: IndexItem -> [[BaseHtml]]
  itemToHtml (ModuleItem (ModuleIndex name author pack link des)) =
    [[BaseStruct "div" [("onclick","window.location='" ++ link ++ "';")]
        [h3 [htxt $ "module " ++ name],
         htxt "author: ", bold [htxt author], breakline,
         htxt "package: ", bold [htxt pack], breakline,
         htxt des]
    ]]
  itemToHtml (FunctionItem (FunctionIndex name modName pack sig _ _ link des)) =
    let sname = if null name || isAlpha (head name)
                  then name
                  else "(" ++ name ++ ")" in
    [[BaseStruct "div" [("onclick","window.location='" ++ link ++ "';")]
        [bold [htxt $ sname ++ " :: " ++ prettySigs sig], breakline,
         --htxt (show sig), breakline,
         htxt "defined in module: ", bold [htxt modName], breakline,
         htxt "of package: ", bold [htxt pack], breakline,
         htxt des]
    ]]
  itemToHtml (TypeItem (TypeIndex name modName pack isClass _ link des)) =
    [[BaseStruct "div" [("onclick","window.location='" ++ link ++ "';")]
        [h3 [htxt $ (if isClass then "class " else "data ") ++ name],
         htxt "defined in module: ", bold [htxt modName], breakline,
         htxt "of package: ", bold [htxt pack], breakline,
         htxt des]
    ]]

-- The description of Currygle in HTML format.
currygleDescription :: [BaseHtml]
currygleDescription =
  [ h4 [htxt "Options to restrict the search:"]
  , headedTable (map (\(x,y) -> [[htxt x], [htxt y]])
      [ ("Option:", "Explanation:")
      , (":module <module name>"
        , "all modules containing <module name> in their name")
      , (":function <function name>"
        , "all functions containing <function name> in their name")
      , (":class <class name>"
        , "all classes containing <class name> in their name")
      , (":type <type name>"
        , "all types containing <type name> in their name")
      , (":inmodule <module name>"
        , "all entities in modules containing <module name> in their name")
      , (":inpackage <package name>"
        , "all entities in packages containing <package name> in their name")
      , (":author <author name>"
        , "all modules whose author contains <author name> in their name")
      , (":signature <signature>"
        , "all functions containing <signature> in their signature, " ++
          "or types containing a constructor containing <signature>")
      , (":deterministic", "all deterministic operations")
      , (":nondeterministic", "all non-deterministic operations")
      , (":flexible", "all flexible operations")
      , (":rigid", "all rigid operations")
      ])
      `addClass` "table table-striped table-sm"
  , par [ htxt "The keywords  AND, OR, and NOT can be used as binary infix "
        , htxt "operators to combine options."
        , htxt "Options can be enclosed in curly braces to allow nested "
        , htxt "expressions." ]
  , par [ htxt "The option keywords can be abbreviated where the abbreviation "
        , htxt "should be unique. If an abbreviation is not unique, "
        , htxt "the first possibility according to this table is taken." ]
  , par [ htxt "Example: the following queries show all non-deterministic "
        , htxt "operations defined in the standard prelude:", breakline
        , code [htxt ":inmodule Prelude AND :nondeteterministic"], breakline
        , code [htxt ":inm Prelude AND :nondet"], breakline
        , code [htxt ":i Prelude AND :n"]
        ]
  ]

------------------------------------------------------------------------------
-- The standard HTML page of Currgle:
curryglePage :: [BaseHtml] -> HtmlPage
curryglePage contents =
  HtmlPage "Curr(y)gle"
    ([pageEnc "utf-8", responsiveView] ++
     [pageLinkInfo [("rel","shortcut icon"), ("href",favIcon)]] ++
     map pageCSS cssIncludes)
    [htmlStruct "nav"  -- top navigation bar
      [("class","navbar navbar-expand-md navbar-dark fixed-top bg-primary")]
      [href "?" [h1 [htxt "Curr(y)gle"]] `addClass` "navbar-brand",
       formElemWithAttrs searchForm
         [("class","form-inline flex-fill"),
          ("title",
           "Query to search entities occurring in modules of Curry packages")]],
     blockstyle "container-fluid" $
       [blockstyle "row" [blockstyle (bsCols 12) contents],
        hrule,
        footer currygleFooter `addAttrs`
         [("style","text-align:center; background-color:hsl(0, 0%, 80%);")]
       ]]
 where
  responsiveView =
    pageMetaInfo
      [("name","viewport"),
       ("content","width=device-width, initial-scale=1, shrink-to-fit=no")]
  bsCols n = "col-sm-" ++ show n ++ " " ++ "col-md-" ++ show n

currygleFooter :: [BaseHtml]
currygleFooter =
  [par [htxt $ "Curr(y)gle version " ++ packageVersion ++ ", powered by ",
        ehrefDarkBadge curryURI [htxt "Curry"], nbsp,
        image "images/Curry.ico" "Curry"]]

--- The base URL where Bootstrap4 style files etc are stored.
baseBT4 :: String
baseBT4 = "bt4"

-- The URL of the favicon.
favIcon :: String
favIcon = baseBT4 </> "img" </> "favicon.ico"

-- The CSS includes.
cssIncludes :: [String]
cssIncludes =
  map (\n -> baseBT4 </> "css" </> n ++ ".css") ["bootstrap.min","curry"]

curryURI :: String
curryURI = "http://www.curry-lang.org/"

------------------------------------------------------------------------------
