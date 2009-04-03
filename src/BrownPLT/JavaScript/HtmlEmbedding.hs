module BrownPLT.JavaScript.HtmlEmbedding
  ( JsHtml
  , ParsedJavaScript
  , ParsedJsHtml
  ) where

import BrownPLT.Html.Syntax(Html,Script,parseScriptBlock,parseInlineScript,
                   parseAttributeScript)
import BrownPLT.JavaScript.Parser(parseScript)
import BrownPLT.JavaScript.Syntax(JavaScript)
--import JavaScript.PrettyPrint -- for the instance declaration
import Text.ParserCombinators.Parsec (SourcePos)

type JsHtml a = Html SourcePos (JavaScript a)

type ParsedJavaScript = JavaScript SourcePos

type ParsedJsHtml = JsHtml SourcePos

instance Script ParsedJavaScript where
  parseScriptBlock attrs = do
    parseScript
  parseInlineScript = Nothing
  parseAttributeScript = Nothing
    
  
