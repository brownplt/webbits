-- |Pretty-printer for HTML.  This modules exports no names.  It only defines
-- instances of 'PrettyPrintable' for HTML. 
module BrownPLT.Html.PrettyPrint
  ( html
  , renderHtml
  ) where

import qualified Data.List as List
import qualified Data.Char as Char
import Text.PrettyPrint.HughesPJ

import BrownPLT.Html.Syntax

renderHtml :: Script s => Html a s -> String
renderHtml = render.html

vert [] = empty
vert [doc] = doc
vert (doc:docs) = doc <> text "<!--" $+$
                  text "-->" <> vert docs

attr :: Script s => Attribute a s -> Doc
attr a = case a of
  Attribute name value _ ->
    text name <> equals <> doubleQuotes (text value)
  AttributeExpr _ n v "" ->
    text n <> equals <> text "{!" <+> prettyPrintScript v <+> text "!}"
  AttributeExpr _ n v d ->
    text n <> equals <> text "{!" <+> prettyPrintScript v <+> text "|||" <+> 
    text d <+> text "!}"

html :: Script s => Html a s -> Doc
html x = case x of
  -- The <script> tag must be terminated by </script>.
  -- <script lang= src= /> doesn't work.
  Element name attrs children _ ->
    -- WARNING: Spacing is very sensitive
    text "<" <> text name <+> hsep (map attr attrs) <> text ">"  -- opening
      $$ (nest 2 (vcat (map html children)))                     -- body
      $$ text "</" <> text name <> text ">"                    -- closing
  -- Horizontally aligned material that is vertically represented in source.
  HtmlSeq xs -> vert (map html xs)
  Text str _ -> text (skipWs str) where
    skipWs str = List.dropWhile Char.isSpace str
  Comment str _ -> text "<!--" <+> text str <+> text "-->"
  ProcessingInstruction str _ -> text "<?" <> text str <> text ">"
  Script s _ -> prettyPrintScript s
  InlineScript script _ "" ->
    text "{!" <+> prettyPrintScript script <+> text "!}"
  InlineScript script _ init -> 
    text "{!" <+> prettyPrintScript script <+> text "|||" <+> 
    text init <+> text "!}"

