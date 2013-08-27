module Test.ExpressionTests (test_ecmascript5_expression) where

import Test.Tasty
import Test.Tasty.HUnit

import Language.ECMAScript5.Syntax.Annotations (reannotate)
import Language.ECMAScript5.Syntax

import Language.ECMAScript5.PrettyPrint
import Language.ECMAScript5.Parser


deannotate :: Positioned Expression -> Expression ()
deannotate = reannotate $ const ()

infix 1 $$
($$) = ($)

infixr 0 $:
($:) = (:)

parseTest :: String -> Expression () -> Assertion
parseTest str ast = 
  case parse expression "" str of
    Right res -> assertEqual "" ast (deannotate res)
    Left err -> assertFailure $ "Unexpected parse error: " ++ show err
    

test_ecmascript5_expression = testGroup "Expression tests" unitTests
             
unitTests = 
     testCase "double negation" $$
       parseTest "!!x" (NumLit () (Left 3))
  $: testCase "prefix/postfix precedence" $$
       parseTest "!x++" (PrefixExpr () PrefixLNot (UnaryAssignExpr () PostfixInc (VarRef () (Id () "x"))))
  $: []
       
run = defaultMain test_ecmascript5_expression