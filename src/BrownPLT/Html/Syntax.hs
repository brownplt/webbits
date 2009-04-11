-- |Datatypes for HTML parameterized over an annotation type and a script type.
module BrownPLT.Html.Syntax 
  (
  -- * HTML Data Structures
    HtmlId,AttributeValue, Attribute (..), Html (..)
  -- * The Script class
  , Script (..)
  -- * Miscellaneous Functions
  , attributeValue, attributeUpdate, attributeSet, isAttributeExpr
  ) where

import Text.ParserCombinators.Parsec (CharParser, SourcePos)
import Text.PrettyPrint.HughesPJ (Doc)
import Data.Generics (Data, Typeable)

--------------------------------------------------------------------------------
-- Types

type HtmlId = String
type AttributeValue = String

data Attribute a s
  = Attribute HtmlId AttributeValue a
  | AttributeExpr a HtmlId s String
  deriving (Show,Eq,Typeable,Data)

data Html a sc
  = Element HtmlId [Attribute a sc] [Html a sc] a
  | Text String a
  | Comment String a
  | HtmlSeq [Html a sc] -- ^must be a non-empty list
  | ProcessingInstruction String a
  | InlineScript sc a String
  | Script sc a
  deriving (Show,Eq,Typeable,Data)
  
--------------------------------------------------------------------------------
-- The Script class

-- |A type 't' of the 'Script' class can be parsed using 'Parsec'.  't' is of
-- kind '* -> *', as the parsed AST should be annotated with souce locations
-- (see 'Text.ParserCombinators.Parsec.SourcePos').
--
-- The big deal here is that we can embed a parser for some scripting language,
-- (e.g. Javascript) into this HTML parser with ease, while preserving source
-- locations.  The Html datatype is parameterized over a script parser (an
-- instance of Script).
class Script t where
  prettyPrintScript :: t -> Doc
  parseScriptBlock:: [Attribute SourcePos t] -> CharParser a t
  -- An inline script parser, which may be Nothing if the scripting language
  -- does not support inline scripts.
  parseInlineScript:: Maybe (CharParser a t)
  -- A parser for script-expressions defined inline as attribute values.
  parseAttributeScript:: Maybe (CharParser a t)
  
--------------------------------------------------------------------------------
-- HTML navigation

isAttributeExpr (AttributeExpr _ _ _ _) = True
isAttributeExpr _                       = False

-- |Returns the value of the attribute in the list, or 'Nothing' if it doesn't
-- exist of the value is an inline-expression.
attributeValue:: HtmlId -> [Attribute a s] -> Maybe String
attributeValue name [] = Nothing
attributeValue name ((AttributeExpr pos name' expr init):rest) =
  if name == name' then Nothing
                   else attributeValue name rest
attributeValue name ((Attribute name' value _):rest) =
  if name == name' then Just value
                   else attributeValue name rest

attributeSet:: HtmlId -> String -> [Attribute a s]  -> [Attribute a s]
attributeSet n v attrs = attributeUpdate n (\_ -> v) attrs

attributeUpdate:: HtmlId -> (String -> String) -> [Attribute a s] 
               -> [Attribute a s]
attributeUpdate n f [] = 
  [Attribute n (f "") (error "attributeUpdate--no value")] -- TODO: undefined?!
attributeUpdate n _ ((AttributeExpr _ _ _ _):_) =
  error $ "attributeUpdate: " ++ n ++ " is an expression-attribute."
attributeUpdate n f ((Attribute n' v p):attrs) =
  if n' == n then (Attribute n (f v) p):attrs
             else (Attribute n' v p):(attributeUpdate n f attrs)

