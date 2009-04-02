-- |Crawls an HTML page for JavaScript
module WebBits.JavaScript.Crawl 
  ( getPageJavaScript
  ) where

import WebBits.Common
import Control.Monad
import Data.Char (toLower)
import Data.Generics
import System.IO
import Text.ParserCombinators.Parsec(parse,setPosition,incSourceColumn,Column,sourceLine,sourceColumn)

import WebBits.Html.Syntax
import qualified WebBits.JavaScript as Js

-- |Returns the source of the script.
scriptSrc:: Js.ParsedJsHtml -> [String]
scriptSrc (Element tag attrs _ _) | (map toLower tag) == "script" =
  case attributeValue "src" attrs of -- TODO: Check for type="javascript"?
    Just ""  -> []
    Just url -> [url]
    Nothing  -> []
scriptSrc _ =
  []

-- |Returns a list of URIs for external Javascript files referenced in the page.
importedScripts:: Js.ParsedJsHtml -> [String]
importedScripts = everything (++) (mkQ [] scriptSrc)

-- |Returns the top-level statements of a script.
scriptText :: Js.ParsedJsHtml -> [Js.ParsedStatement]
scriptText (Script (Js.Script _ stmts) _) = stmts
scriptText _ = []

eventHandlers :: [String]
eventHandlers = ["onload","onclick"]; 
-- ,"onmousemove","onmouseover","onmousedown","onmouseout","onmouseup","onselectstart", "onkeypress"]

attrScript :: Attribute SourcePos Js.ParsedJavaScript 
           -> IO [Js.ParsedStatement]
attrScript (Attribute id val loc) | id `elem` eventHandlers = do
  let eventId = drop 2 id -- drop the "on" prefix
  let scriptText = if "javascript:" `isPrefixOf` val then drop 11 val else val
  let eventListenerPrefix = "addEventListener('" ++ eventId ++ "', function(event) { "
  let prefixLen = length eventListenerPrefix
  let eventListenerText = eventListenerPrefix ++ scriptText ++ " });"
  let parser = do
        setPosition (incSourceColumn loc (-prefixLen))
        Js.parseExpression
  case parse parser (sourceName loc) eventListenerText of
    Left err -> do
      fail $ "Error parsiing JavaScript in an attribute at " ++ show loc ++
             "\nThe script was:\n\n" ++ eventListenerText
    Right e -> return [Js.ExprStmt loc e]
attrScript _ = return []

inpageAttrScripts :: Js.ParsedJsHtml -> IO [Js.ParsedStatement]
inpageAttrScripts = everything (liftM2 (++)) (mkQ (return []) attrScript)

inpageScripts :: Js.ParsedJsHtml -> [Js.ParsedStatement]
inpageScripts = everything (++) (mkQ [] scriptText)

parseJsFile path = do
  text <- readFile path
  case Js.parseScriptFromString path text of
    Left err -> fail (show err)
    Right js -> hPutStrLn stderr ("Read file " ++ path) >> return js

-- |Given an HTML page, crawls all external Javascript files and returns a list
-- of statements, concatenated from all files.
getPageJavascript:: Js.ParsedJsHtml -> IO [Js.ParsedStatement]
getPageJavascript page = do
  let importURIs = importedScripts page
  let inpageJs   = inpageScripts page
  attrScripts <- inpageAttrScripts page
  importedScripts <- mapM parseJsFile importURIs
  return $ (concatMap Js.scriptStatements importedScripts ++ attrScripts) ++ inpageJs

getPageJavaScript:: Js.ParsedJsHtml -> IO [Js.ParsedStatement] -- monomorphism
getPageJavaScript = getPageJavascript
