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
       parseTest "!!x" (PrefixExpr () PrefixLNot (PrefixExpr () PrefixLNot (VarRef () (Id () "x"))))
  $: testCase "prefix/postfix precedence" $$
       parseTest "!x++" (PrefixExpr () PrefixLNot (UnaryAssignExpr () PostfixInc (VarRef () (Id () "x"))))
  $: testCase "> operators" $$
       parseTest "x >>>= y > x >>> 3" (AssignExpr () OpAssignZfRShift (VarRef () (Id () "x")) (InfixExpr () OpGT (VarRef () (Id () "y")) (InfixExpr () OpZfRShift (VarRef () (Id () "x")) (NumLit () (Left 3)))))
  $: testCase "& operators" $$
       parseTest "a &= x & y && z" (AssignExpr () OpAssignBAnd (VarRef () (Id () "a")) (InfixExpr () OpLAnd (InfixExpr () OpBAnd (VarRef () (Id () "x")) (VarRef () (Id () "y"))) (VarRef () (Id () "z"))))
  $: testCase "+ operators" $$
       parseTest "!++x" (PrefixExpr () PrefixLNot (UnaryAssignExpr () PrefixInc (VarRef () (Id () "x"))))
  $: []
       
run = defaultMain test_ecmascript5_expression