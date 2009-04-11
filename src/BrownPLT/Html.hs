-- |Rexports various modules of the HTML library.  It's best to use this in lieu
-- of selectively importing the following libraries.
module BrownPLT.Html
  ( module BrownPLT.Html.Syntax
  -- PermissiveParser
  , renderHtml
  , parseHtmlFromFile
  , parseHtmlFromString
  ) where
  
import BrownPLT.Html.Syntax
import BrownPLT.Html.PermissiveParser
import BrownPLT.Html.PrettyPrint -- no names, only instances
import BrownPLT.Html.Instances -- no names, only instances
