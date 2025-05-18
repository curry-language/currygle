----------------------------------------------------------------------
--- This module handles writing the html pages for currygle
---
--- @author Helge Knof (with changes by Michael Hanus)
--- @version May 2025
----------------------------------------------------------------------

module HtmlGenerator
    where

import Data.List      ( intersperse )
import System.IO

import HTML.Base
import System.Process ( system )

import Index.IndexItem
import Index.Indexer
import Index.Signature
import PackageConfig      ( packageVersion )
import Settings
import Search.Search
import Search.SearchQuery
import Server             ( searchClient )

main :: IO HtmlPage
main = getDefaultPage

-- Gets an HtmlRef to a textfield, an HtmlEnv, and creates an HtmlPage from it,
-- showing the search result for the String in the HtmlEnv under HtmlRef
getResultPage :: Bool -> String -> IO HtmlPage
getResultPage withserver searchtxt
  | all isSpace searchtxt -- no query terms present
  = getDefaultPage
  | otherwise
  = case parseSearchText searchtxt of
      Nothing  -> getDefaultPage
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
                            [(items,_)] -> getSearchPage searchtxt items
                            _           -> getDefaultPage
          else resultFromIndex query
 where
  resultFromIndex query = do
    index <- readIndex indexDirPath
    getSearchPage (searchtxt ++ " (search server not reachable)")
                  (map fst (currygleSearch index query))

getSearchPage :: String -> [IndexItem] -> IO HtmlPage
getSearchPage search items = do
  return $ page "Currygle2" $
    [currygle2styles,
     addAttr (formElem searchForm) ("class", "search"),
     htxt $ "Last search: " ++ search, breakline,
     htxt $ "showing " ++ show (min (length items) maxSearchResults) ++
            " of " ++ show (length items) ++ " results"
    ]
    ++ getResultBody (take maxSearchResults items)
    ++ [currygleFooter]

getDefaultPage :: IO HtmlPage
getDefaultPage = return $ page "Currygle2"
            ([currygle2styles
            , addAttr (formElem searchForm) ("class", "search")
            , getDescription
            , currygleFooter])

searchForm :: HtmlFormDef ()
searchForm = simpleFormDefWithID "HtmlGenerator.searchForm"
  [ addAttr (textField ref "") ("class", "searchbar"),
    addAttr (button "Search!" (\env -> getResultPage True (env ref)))
            ("class", "searchbutton")
  ]
 where ref free

getResultBody :: [IndexItem] -> [BaseHtml]
getResultBody items = (map indexItemToHtml items)
  where
    indexItemToHtml :: IndexItem -> BaseHtml
    indexItemToHtml (ModuleItem (ModuleIndex name author pack link des)) =
       BaseStruct "div" [("onclick","window.location='"++link++"';"),("class", "curryentity")]
       [ par [
          h3 [htxt $ "module " ++ name],
          htxt $ "author: " ++ author, breakline,
          htxt $ "package: " ++ pack, breakline,
          htxt $ take 100 des, breakline]
        ]
    indexItemToHtml (FunctionItem (FunctionIndex name modName pack sig _ _ link des)) =
       BaseStruct "div" [("onclick","window.location='"++link++"';"),
                         ("class", "curryentity")]
        [ par [
            htxt $ name ++ " :: " ++ prettySigs sig, breakline,
            h3 [htxt $ "function " ++ name],
            htxt $ "module: " ++ modName, breakline,
            htxt $ "package: " ++ pack, breakline,
            htxt $ take 100 des, breakline]
        ]
    indexItemToHtml (TypeItem (TypeIndex name modName pack isClass _ link des)) =
        BaseStruct "div" [("onclick","window.location='"++link++"';"),
                          ("class", "curryentity")]
          [ par [
              h3 [htxt $ (if isClass then "class " else "data ") ++ name],
              htxt $ "module: " ++ modName, breakline,
              htxt $ "package: " ++ pack, breakline,
              htxt $ take 100 des,breakline]
          ]

currygleFooter :: BaseHtml
currygleFooter =
  addAttr
    (footer [ BaseStruct "style" [] [BaseText "p {text-align: center}"]
            , BaseStruct "p" [("class","FooterText")]
                [htxt $ "Curr(y)gle version " ++ packageVersion]
            , BaseStruct "p" [("class","FooterText")]
                [ htxt "Powered by Curry", nbsp
                , BaseStruct "img" [ ("src", "images/Curry.ico")
                                    , ("class","center")] []]])
    ("style", "background-color:Orange")
  
currygle2styles :: BaseHtml
currygle2styles = BaseStruct "style" [] [BaseText 
                    (".search{\n"++
                    "  background-color: orange;\n"++
                    "}\n"++
                    ".searchbar{\n"++
                    "  background-color: white;\n"++
                    "  border-style: solid;\n"++
                    "  border-color: black;\n"++
                    "  font-weight: 500;\n"++
                    "  padding: .375rem .75rem;\n"++
                    "  width: 70%\n"++
                    "}\n"++
                    ".searchbutton{\n"++
                    "  background-color: lightgray;\n"++
                    "  font-weight: 500;\n"++
                    "  padding: .375rem .75rem;\n"++
                    "  width: 25%\n"++
                    "}\n"++
                    ".result{\n"++
                    "  background-color: lightyellow;\n"++
                    "  border-style: double;\n"++
                    "}\n"++
                    ".curryentity{\n"++
                    "  text-align: center;\n"++
                    "  font-size: 12px;\n"++
                    "  border-style: solid;\n"++
                    "  border-color: black;\n"++
                    "  border-radius: 25px;\n"++
                    "}\n"++
                    ".entitylink{\n"++
                    "  display: block;\n"++
                    "  height: 100%;\n"++
                    "  width: 100%;\n"++
                    "}\n")
  ]

getDescription :: BaseHtml
getDescription = par $ intersperse breakline $ map htxt
  [ "Possible search queries:  "
  , ":module <module name> finds all modules containing <module name> in their name,  abbreviation :m  "
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
