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
import System.FilePath ( (</>) )
import System.Process  ( system )

import Index.IndexItem
import Index.Indexer
import Index.Signature
import PackageConfig ( packageVersion )
import Settings
import Search.Search
import Search.SearchQuery
import Server ( searchClient )

main :: IO HtmlPage
main = return defaultPage

-- Default page with Currygle description.
defaultPage :: HtmlPage
defaultPage = curryglePage currygleDescription

-- Gets an HtmlRef to a textfield, an HtmlEnv, and creates an HtmlPage from it,
-- showing the search result for the String in the HtmlEnv under HtmlRef
getResultPage :: Bool -> String -> IO HtmlPage
getResultPage withserver searchtxt
  | all isSpace searchtxt -- no query terms present
  = return defaultPage
  | otherwise
  = case parseSearchText searchtxt of
      Nothing  -> return defaultPage
      Just query ->
        if withserver
          then do
            mbres <- catch (searchClient searchtxt >>= return . Just)
                           (\_ -> return Nothing)
            case mbres of
              Nothing  -> do -- try to restart server:
                             system "make restart >> SERVER.LOG"
                             resultFromIndex query
              Just res -> case reads res of
                            [(items,_)] -> return $ searchPage searchtxt items
                            _           -> return defaultPage
          else resultFromIndex query
 where
  resultFromIndex query = do
    index <- readIndex indexDirPath
    return $ searchPage (searchtxt ++ " (search server not reachable)")
                        (currygle2search index query)

searchPage :: String -> [IndexItem] -> HtmlPage
searchPage search items = curryglePage $
  [ htxt $ "Search results for '" ++ search ++ "': "
  , htxt $ "found " ++ show num ++ " entities" ++
           if length items <= maxSearchResults
             then ""
             else " (showing " ++ show (min num maxSearchResults) ++ " results)"
  , searchResults $ take maxSearchResults items]
 where num = length items

--- The search form consisting of a field and search button.
searchForm :: HtmlFormDef ()
searchForm = simpleFormDefWithID "WebQuery.searchForm"
  [ textField ref "" `addAttrs`
      [("class", "form-control mr-sm-2 flex-fill"),
       ("placeholder","Search in Curry packages")],
    addAttr (button "Search!" (\env -> getResultPage True (env ref)))
            ("class", "btn btn-outline-success my-2 my-sm-0")
  ]
 where ref free

-- Format search results.
searchResults :: [IndexItem] -> BaseHtml
searchResults items =
  table (map indexItemToHtml items)  `addClass` "table table-hover"
 where
  indexItemToHtml :: IndexItem -> [[BaseHtml]]
  indexItemToHtml (ModuleItem (ModuleIndex name author pack link des)) =
    [[BaseStruct "div" [("onclick","window.location='" ++ link ++ "';")]
        [h3 [htxt $ "module " ++ name],
         htxt "author: ", bold [htxt author], breakline,
         htxt "package: ", bold [htxt pack], breakline,
         htxt des]
    ]]
  indexItemToHtml (FunctionItem (FunctionIndex name modName pack sig _ _ link des)) =
    [[BaseStruct "div" [("onclick","window.location='" ++ link ++ "';")]
        [bold [htxt $ name ++ " :: " ++ prettySigs sig], breakline,
         --htxt (show sig), breakline,
         htxt "defined in module: ", bold [htxt modName], breakline,
         htxt "of package: ", bold [htxt pack], breakline,
         htxt des]
    ]]
  indexItemToHtml (TypeItem (TypeIndex name modName pack isClass _ link des)) =
    [[BaseStruct "div" [("onclick","window.location='" ++ link ++ "';")]
        [h3 [htxt $ (if isClass then "class " else "data ") ++ name],
         htxt "defined in module: ", bold [htxt modName], breakline,
         htxt "of package: ", bold [htxt pack], breakline,
         htxt des]
    ]]

currygleDescription :: [BaseHtml]
currygleDescription =
  [ h4 [htxt "Options to restrict the restrict the search:"]
  , headedTable (map (\(x,y,z) -> [[htxt x], [htxt y], [htxt z]])
      [ ("Long form:", "Abbreviation:", "Explanation:")
      , (":module <module name>", ":m <module name>", "all modules containing <module name> in their name")
      , (":function <function name>", ":f <function name>", "all functions containing <function name> in their name")
      , (":class <class name>", ":c <class name>", "all classes containing <class name> in their name")
      , (":type <type name>", ":t <type name>", "all types containing <type name> in their name")
      , (":inmodule <module name>", ":im <module name>", "all entities in modules containing <module name> in their name")
      , (":inpackage <package name>", ":ip <package name>", "all entities in packages containing <package name> in their name")
      , (":author <author name>", ":a <author name>", "all modules whose author contains <author name> in their name")
      , (":signature <signature>", ":s <signature>", "all functions containing <signature> in their signature, or types containing a constructor containing <signature>")
      , (":det", ":det", "all deterministic functions")
      , (":nondet", ":nd", "all non-deterministic functions")
      , (":flexible", ":fl", "all flexible functions")
      , (":rigid", ":r", "all rigit functions")
      ])
      `addClass` "table table-striped table-sm"
  , par [ htxt "The keywords  AND, OR and NOT can be used as binary infix "
        , htxt "operators to combine several options."
        , htxt "Options can be enclosed in curly braces to allow nested "
        , htxt "expressions." ]
  ]

currygleDescription' :: BaseHtml
currygleDescription' = par $ intersperse breakline $ map htxt
  [ "Possible search queries:"
  , ":module <module name> or :m <module name>: finds all modules containing <module name> in their name"
  , ":function <function name> finds all functions containing <function name> in their name,  abbreviation :f  "
  , ":class <class name> finds all classes containing <class name> in their name,  abbreviation :c  "
  , ":type <type name> finds all types containing <type name> in their name,  abbreviation :t  "
  , ":inmodule <module name> finds all entities in modules containing <module name> in their name,  abbreviation :im  "
  , ":inpackage <package name> finds all entities in packages containing <package name> in their name,  abbreviation :ip  "
  , ":author <author name> finds all modules whose author contains <author name> in their name,  abbreviation :a  "
  , ":signature <signature> takes a signature like it is written in Curry code and returns all functions containing the signature in their signature, or types containing a constructor containing the signature. abbreviation :s  "
  , ":det returns all deterministic functions  "
  , ":nondet returns all non-deterministic functions, abbreviation :nd  "
  , ":flexible returns all flexible functions, abbreviation :fl  "
  , ":rigid returns all rigid functions, abbreviation :r  "
  , "  "
  , "All these searches can be connected with the junctors AND, OR and NOT"
  , "{} can be used to structure the query."
  ]

------------------------------------------------------------------------------
-- Standard form in Curry pages:
curryglePage :: [BaseHtml] -> HtmlPage
curryglePage contents =
  HtmlPage "Currygle"
    ([pageEnc "utf-8", responsiveView] ++
     [pageLinkInfo [("rel","shortcut icon"), ("href",favIcon)]] ++
     map pageCSS cssIncludes)
    [htmlStruct "nav"  -- top navigation bar
      [("class","navbar navbar-expand-md navbar-dark fixed-top bg-dark")]
      [href "?" [h1 [htxt "Currygle"]] `addClass` "navbar-brand",
       formElemWithAttrs searchForm
         [("class","form-inline flex-fill"),
          ("title", "Query to search in module of Curry packages")]],
     blockstyle "container-fluid" $
       [blockstyle "row" [blockstyle (bsCols 12) contents],
        hrule,
        footer currygleFooter `addAttrs`
         [("style","text-align:center; background-color:hsl(0, 0%, 96%);")]
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
        href curryURI [htxt "Curry"], nbsp,
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
