----------------------------------------------------------------------
--- This module handles writing the html pages for currygle
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module WebInterface --( main, searchForm )
 where

import Data.List      ( init, intercalate, isPrefixOf, last, split )
import System.IO

import HTML.Base
import HTML.Styles.Bootstrap4 ( ehrefDarkBadge, kbdInput, hrefInfoBadge )
import Network.URL      ( string2urlencoded, urlencoded2string )
import System.Directory ( doesFileExist )
import System.FilePath  ( (</>) )
import System.IOExts    ( evalCmd )
import System.Process   ( system )
import Text.Markdown    ( markdownText2HTML )

import Index.Helper       ( strip )
import Index.Item
import Index.Indexer
import Index.Signature
import PackageConfig      ( packageVersion )
import Options
import Settings
import Search.Execute
import Search.Query
import Server             ( profilingSearchClient, searchClient )

main :: IO HtmlPage
main = fmap urlencoded2string getUrlParameter >>= runWithUrlParams

--- Execute script with URL parameter:
--- - no parameter: interactive mode
--- - `:status`: show status of server process
--- - `:all`: show all results of query provided after `:all`
--- - otherwise: run query provided as parameter
runWithUrlParams :: String -> IO HtmlPage
runWithUrlParams uparam
  | null uparam             = return defaultPage
  | uparam == ":status"     = getStatusPage
  | take 4 uparam == ":all" = getResultPage True True (drop 4 uparam)
  | otherwise               = getResultPage True False uparam

-- Default page with Currygle description.
defaultPage :: HtmlPage
defaultPage = curryglePage currygleDescription

-- Page showing status of Currygle server process.
getStatusPage :: IO HtmlPage
getStatusPage = do
  (_,out,_) <- evalCmd "make" ["status"] ""
  return $ curryglePage
    [h1 [htxt "Status of the Currygle server"], verbatim out]

-- For a given search string (third argument), performs the search
-- by contacting the search server (or restart it if not reachable and
-- do the search locally), and shows the results in a web page.
-- If the second argument is `True`, show all results.
getResultPage :: Bool -> Bool -> String -> IO HtmlPage
getResultPage withserver showall searchtxt
  | all isSpace searchtxt -- no query terms present
  = return defaultPage
  | otherwise
  = do let (fuzzy,squery) = if ":fuzzy" `isPrefixOf` searchtxt
                              then (True, strip (drop 6 searchtxt))
                              else (False,searchtxt)

       case parseSearchText squery of
          Nothing  -> return defaultPage
          Just query ->
            if withserver
              then do
                mbres <- catch
                          (profilingSearchClient fuzzy squery >>= return . Just)
                          (\_ -> return Nothing)
                case mbres of
                  Nothing  -> do -- try to restart server:
                                system "make restart >> SERVER.LOG"
                                resultFromIndex fuzzy query
                  Just res -> case reads res of
                    [((items,et),_)] -> getItemspage fuzzy query "" items et
                    _                -> return defaultPage
              else resultFromIndex fuzzy query
 where
  resultFromIndex fuzzy query = do
    index <- readIndex indexDirPath
    getItemspage fuzzy query " (slow search since server not reachable)"
                 (currygleSearch fuzzy index query) 0

  getItemspage fuzzy query note items etime = do
    htmlresults <- searchResults2HTML showall $ take maxresults items
    return $ curryglePage $
      [ par $
          [ italic [htxt $ "Search results for "]
          , kbdInput [htxt $ if fuzzy then ":fuzzy " else "",
                      htxt (prettySearchQuery query)]
          , htxt $ note ++ ": "
          , htxt $ "found "
          , htxt $ case num of 0 -> "no entity"
                               1 -> "one entity"
                               _ -> show num ++ " entities"
          , htxt $ if etime==0 then " " else " in " ++ show etime ++ " ms " ] ++
          if showall
            then []
            else [ htxt "("
                , if num <= maxresults
                    then htxt ""
                    else htxt $ "showing " ++ show (min num maxresults) ++
                                " results, "
                , showAllButton, htxt ")"]
      , htmlresults]
   where
    num           = length items
    maxresults    = if showall then num else maxSearchResults
    showAllButton = hrefInfoBadge
                      ('?' : string2urlencoded (":all" ++ searchtxt))
                      [htxt $ "show " ++
                              if num>maxresults then "all" else "scores"]

--- The search form consisting of a field and search button.
searchForm :: HtmlFormDef ()
searchForm = simpleFormDefWithID "WebInterface.searchForm"
  [ textField ref "" `addAttrs`
      [("class", "form-control mr-sm-2 flex-fill"),
       ("placeholder","Search in Curry packages")],
    addAttr (button "Search!" (\env -> getResultPage True False (env ref)))
            ("class", "btn btn-outline-light my-2 my-sm-0")
  ]
 where ref free

-- Format search results.
searchResults2HTML :: Bool -> [(IndexItem,Int)] -> IO BaseHtml
searchResults2HTML showscore items = do
  hitems <- mapM itemToHtml items
  return $ (table hitems `addClass` "table table-sm table-hover")
 where
  itemHeader header score = h5 $
    [htxt header, nbsp] ++
    if showscore
      then [nbsp, style "badge badge-dark" [htxt $ "score " ++ show score]]
      else []

  -- format markdown text:
  ppDesc = markdownText2HTML

  addLink docurl curryinforefbutton hitems =
    [[hrefInfoBadge docurl [htxt "Documentation"], nbsp] ++
      (if null srcurl then []
                      else [hrefInfoBadge srcurl [htxt "Source code"], nbsp]) ++
      curryinforefbutton ++ 
     [blockstyle "resultitem" [href docurl [block hitems]]]
    ]
   where srcurl = docUrl2SourceCodeUrl docurl

  itemToHtml :: (IndexItem,Int) -> IO [[BaseHtml]]
  itemToHtml (ModuleItem name author pack des, score) = do
    let docurl = moduleDocumentationUrl pack name
    return $ addLink docurl [] $
      [itemHeader ("module " ++ name) score,
       htxt "authors: ", bold [htxt author], breakline,
       htxt "package: ", bold [htxt pack], breakline] ++ ppDesc des
  itemToHtml (FunctionItem name modName pack sig _ _ des, score) = do
    let cifname = operationAnalysisFile pack modName name
    ciexists <- if null cifname then return False else doesFileExist cifname
    let sname = if null name || isAlpha (head name)
                  then name
                  else "(" ++ name ++ ")"
        docurl = entityDocumentationUrl pack modName name
    return $ addLink docurl
            (if ciexists then function2CurryInfoRef pack modName name else []) $
      [itemHeader (sname ++ " :: " ++ prettySigs sig) score,
       htxt "defined in module: ", bold [htxt modName], breakline,
       htxt "of package: ", bold [htxt pack], breakline] ++ ppDesc des
  itemToHtml (TypeItem name modName pack isclass cs des, score) = do
    let docurl = entityDocumentationUrl pack modName name
    return $ addLink docurl [] $
      [itemHeader (if isclass then "class " ++ name else "data " ++ ppType)
                  score,
       htxt "defined in module: ", bold [htxt modName], breakline,
       htxt "of package: ", bold [htxt pack], breakline] ++ ppDesc des
   where
    -- pretty print the type from the TypeIndex
    ppType = case cs of
      []     -> name ++ " ="
      (c1:_) -> prettySig False (last (snd c1)) ++ " = " ++
                intercalate " | "
                  (map (\(c,ts) -> unwords (c : map (prettySig True) (init ts)))
                       cs)

  function2CurryInfoRef pkgid mname ename =
    let url = operationAnalysisURL pkgid mname ename in
    if null url then [] else [hrefInfoBadge url [htxt "Analysis Information"]]

-- The description of Currygle in HTML format.
currygleDescription :: [BaseHtml]
currygleDescription =
  [blockstyle "row justify-content-center"
    [blockstyle "col-sm-12 col-md-10 col-lg-6" currygleHelpText]]

currygleHelpText :: [BaseHtml]
currygleHelpText =
  [ h4 [htxt "Options to restrict the search:"]
  , headedTable (map (\(o,s) -> [[bold [htxt o]], [htxt s]])
      [ ("Option:", "Searches for")
      , (":function <f>"
        , "functions or data constructors containing <f> in their name")
      , (":module <m>", "modules containing <m> in their name")
      , (":class <c>",     "classes containing <c> in their name")
      , (":type <t>",      "types containing <t> in their name")
      , (":inmodule <m>",  "entities of modules containing <m> in their name")
      , (":inpackage <p>", "entities of packages containing <p> in their name")
      , (":author <n>",    "modules with an author name containing <n>")
      , (":signature <s>"
        , "functions or types with constructors containing <s> in their " ++
          "signature, e.g., \":signature [a] -> [b]\" or " ++
          "\":signature a -> String\"")
      , (":deterministic",    "deterministic operations")
      , (":nondeterministic", "non-deterministic operations")
      , (":flexible",         "flexible operations")
      , (":rigid",            "rigid operations")
      ])
      `addClass` "table table-striped table-sm"
  , par [ htxt "Search terms are not case sensitive. "
        , htxt "The keywords  AND, OR, and NOT can be used as binary infix "
        , htxt "operators to combine options."
        , htxt "Options can be enclosed in curly braces to allow nested "
        , htxt "expressions." ]
  , par [ htxt "The option keywords can be abbreviated where the abbreviation "
        , htxt "should be unique. If an abbreviation is not unique, "
        , htxt "the first possibility according to this table is taken. "]
  , par [ italic [htxt "Example:"]
        , htxt " the following queries show all non-deterministic "
        , htxt "operations defined in the standard prelude:", breakline
        , kbdInput [htxt ":inmodule Prelude AND :nondeteterministic"], breakline
        , kbdInput [htxt ":inm Prelude AND :nondet"], breakline
        , kbdInput [htxt ":i Prelude AND :n"]
        ]
  , par [ htxt "If the entire query is prefixed by "
        , kbdInput [htxt ":fuzzy"], htxt ", a fuzzy text search is used "
        , htxt "to find more results without exact matches."
        ]
  ]

-- Explanation text for the search query field:
queryExplain :: String
queryExplain =
  "Query to search in definitions and comments occurring in modules of Curry packages"

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
         [("class","form-inline flex-fill"), ("title",queryExplain)]],
     blockstyle "container-fluid" $
       [blockstyle "row" [blockstyle "col-sm-12 col-md-12" contents],
        hrule,
        footer currygleFooter `addAttrs`
         [("style","text-align:center; background-color:hsl(0, 0%, 80%);")]
       ]]
 where
  responsiveView =
    pageMetaInfo
      [("name","viewport"),
       ("content","width=device-width, initial-scale=1, shrink-to-fit=no")]

currygleFooter :: [BaseHtml]
currygleFooter =
  [par [htxt $ "Curr(y)gle (Version " ++ packageVersion ++ " of " ++
               currygleDate ++ "), powered by ",
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
