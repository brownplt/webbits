module BrownPLT.Html.RawScript
  ( RawScript (..)
  , parseFromFile
  , parseFromString
  , RawHtml
  ) where

import Data.Generics (Data, Typeable)
import Text.PrettyPrint.HughesPJ (text)
import Text.ParserCombinators.Parsec
import BrownPLT.Html.Syntax
import BrownPLT.Html.PermissiveParser

type RawHtml = Html SourcePos RawScript


data RawScript = RawScript String deriving (Show,Eq,Typeable,Data)


instance Script RawScript where

  prettyPrintScript (RawScript s) = text s
  parseInlineScript = Nothing
  
  parseAttributeScript = Nothing
  
  parseScriptBlock _ = do
    s <- manyTill anyChar (string "</script>")
    return (RawScript s)
    

parseFromString :: String -> RawHtml
parseFromString s = case parseHtmlFromString "" s of
  Left e -> error (show e)
  Right (html,_) -> html
