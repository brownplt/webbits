module BrownPLT.Html.RawScript
  ( RawScript (..)
  , parseFromFile
  , parseFromString
  , RawHtml
  ) where

import Data.Generics (Data)
import Data.Generics (Typeable)
import Text.PrettyPrint.HughesPJ (text)
import Text.ParserCombinators.Parsec
import BrownPLT.Common
import BrownPLT.Html.Html

type RawHtml = Html SourcePos RawScript

data RawScript = RawScript String deriving (Show,Eq,Typeable,Data)

instance Script RawScript where
  parseInlineScript = Nothing
  
  parseAttributeScript = Nothing
  
  parseScriptBlock _ = do
    s <- manyTill anyChar (string "</script>")
    return (RawScript s)
    
instance PrettyPrintable RawScript where
  pp (RawScript s) = text s

parseFromString :: String -> RawHtml
parseFromString s = case parseHtmlFromString "" s of
  Left e -> error (show e)
  Right (html,_) -> html
