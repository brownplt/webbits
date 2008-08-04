module Test.HtmlTest where

import Test.HUnit.Base
import Test.HUnit.Text
import Text.ParserCombinators.Parsec.Pos (initialPos)
import WebBits.Html.RawScript

assertHtmlEqual :: String -> String -> String -> Assertion
assertHtmlEqual str x y= 
  let x' = fmap (const $ initialPos "") parseFromString x
      y' = fmap (const $ initialPos "") parseFromString y
    in assertEqual str x' y'
  

testHtml x y = TestCase (assertHtmlEqual "structural equality" x y)

structureRecoveryTests = TestList
  [ testHtml "<html><body><p>check</body></html>" 
             "<html><body><p>check</p></body></html>"
  , testHtml "<html><body><p>1<p>2<p>3</body></html>"
             "<html><body><p>1</p><p>2</p><p>3</p></body></html>"
  ]
  
  
allTests = TestList
  [ structureRecoveryTests 
  ]
  
main = return allTests
