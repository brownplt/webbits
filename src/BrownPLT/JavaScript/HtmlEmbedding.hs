module BrownPLT.JavaScript.HtmlEmbedding
  ( JsHtml
  , ParsedJavaScript
  , ParsedJsHtml
  ) where

import BrownPLT.Html.Syntax
import BrownPLT.JavaScript.Parser (parseScript)
import BrownPLT.JavaScript.Syntax (JavaScript)
import BrownPLT.JavaScript.PrettyPrint (javaScript)
import Text.ParserCombinators.Parsec (SourcePos)

type JsHtml a = Html SourcePos (JavaScript a)

type ParsedJavaScript = JavaScript SourcePos

type ParsedJsHtml = JsHtml SourcePos

instance Script ParsedJavaScript where
  prettyPrintScript s = javaScript s
  parseScriptBlock attrs = parseScript
  parseInlineScript = Nothing
  parseAttributeScript = Nothing
    
  
