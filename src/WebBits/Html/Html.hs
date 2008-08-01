-- |Rexports various modules of the HTML library.  It's best to use this in lieu
-- of selectively importing the following libraries.
module WebBits.Html.Html
  ( module WebBits.Html.Syntax
  -- PermissiveParser
  , html
  , parseHtmlFromFile
  , parseHtmlFromString
  ) where
  
import WebBits.Html.Syntax
import WebBits.Html.PermissiveParser
import WebBits.Html.PrettyPrint -- no names, only instances
import WebBits.Html.Instances -- no names, only instances
