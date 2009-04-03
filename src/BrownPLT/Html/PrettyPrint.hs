-- |Pretty-printer for HTML.  This modules exports no names.  It only defines
-- instances of 'PrettyPrintable' for HTML. 
module BrownPLT.Html.PrettyPrint
  ( -- this module exports no names
  ) where

import qualified Data.List as List
import qualified Data.Char as Char
import Text.PrettyPrint.HughesPJ

import BrownPLT.Common (PrettyPrintable(..))

import BrownPLT.Html.Syntax

vert [] = empty
vert [doc] = doc
vert (doc:docs) = doc <> text "<!--" $+$
                  text "-->" <> vert docs

instance PrettyPrintable s => PrettyPrintable (Attribute a s) where
  pp (Attribute name value _) =
    text name <> equals <> doubleQuotes (text value)
  pp (AttributeExpr _ n v "") =
    text n <> equals <> text "{!" <+> pp v <+> text "!}"
  pp (AttributeExpr _ n v d) =
    text n <> equals <> text "{!" <+> pp v <+> text "|||" <+> text d 
      <+> text "!}"

instance PrettyPrintable s => PrettyPrintable (Html a s) where
  -- The <script> tag must be terminated by </script>.
  -- <script lang= src= /> doesn't work.
  -- pp (Element name attrs [] _) =
  --   text "<" <> text name <+> hsep (map pp attrs) <+> text "/>"
  pp (Element name attrs children _) =
    -- WARNING: Spacing is very sensitive
    text "<" <> text name <+> hsep (map pp attrs) <> text ">"  -- opening
      $$ (nest 2 (vcat (map pp children)))                     -- body
      $$ text "</" <> text name <> text ">"                    -- closing
  -- Horizontally aligned material that is vertically represented in source.
  pp (HtmlSeq xs) = vert (map pp xs)
  pp (Text str _) =
    text (skipWs str) where
      skipWs str = List.dropWhile Char.isSpace str
  pp (Comment str _) =
    text "<!--" <+> text str <+> text "-->"
  pp (ProcessingInstruction str _) =
    text "<?" <> text str <> text ">"
  pp (Script script _) =
    pp script
  pp (InlineScript script _ "") =
    text "{!" <+> pp script <+> text "!}"
  pp (InlineScript script _ init) =
    text "{!" <+> pp script <+> text "|||" <+> text init <+> text "!}"

