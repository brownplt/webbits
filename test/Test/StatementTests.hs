module Test.StatementTests (tests_ecmascript5_parser) where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Unsafe
import Language.ECMAScript5.Syntax.Annotations (reannotate)
import Language.ECMAScript5.Syntax

import Text.Parsec (SourcePos, errorPos, sourceLine, sourceColumn)

import Language.ECMAScript5.PrettyPrint
import Language.ECMAScript5.Parser

tests_ecmascript5_parser :: TestTree
tests_ecmascript5_parser = testGroup "Parser tests" unitTests

infix 1 $$
($$) = ($)

infixr 0 $:
($:) = (:)

deannotate :: [ParsedStatement] -> [Statement ()]
deannotate = map $ reannotate $ const ()

parseTest :: String -> [Statement ()] -> Assertion
parseTest file ast = 
          do content <- readFile ("test-data/" ++ file ++ ".js")
             let res = parseScriptFromString content
             case res of
               Right value -> assertEqual "Unexpected AST" ast (deannotate value)
               Left parseError -> assertFailure (show parseError)
     
expectedParseFail :: String -> (Int, Int) -> Assertion
expectedParseFail file (expectedLine, expectedCol) = 
  do content <- readFile ("test-data/" ++ file ++ ".js")
     let res = parseScriptFromString content
     case res of 
       Right value -> assertFailure "Expected parse error"
       Left err -> let pos  = errorPos err
                       line = sourceLine pos
                       col  = sourceColumn pos
                   in do
                    assertEqual "Parse failure at wrong line" line expectedLine
                    assertEqual "Parse failure at wrong line" col expectedCol

unitTests =
     testCase "Test function definition" $$
       parseTest "empty-function" 
       [ExprStmt () (FuncExpr () Nothing [] [])]
  $: testCase "Test function returning function" $$
       parseTest "function-return-function" 
       [ExprStmt () (FuncExpr () Nothing [] [ReturnStmt () (Just (FuncExpr () Nothing [] []))])]
  $: testCase "Function w/ body" $$
       parseTest "function-with-body"
       [ExprStmt () (FuncExpr () Nothing [] [VarDeclStmt () [VarDecl () (Id () "x") (Just (CallExpr () (VarRef () (Id () "g")) []))],IfStmt () (InfixExpr () OpEq (VarRef () (Id () "x")) (NumLit () (Left 10))) (BlockStmt () [ExprStmt () (AssignExpr () (VarRef () (Id () "x")) OpAssign (NumLit () (Left 20)))]) (EmptyStmt ()),EmptyStmt ()])]
  $: testCase "Two statements" $$
       parseTest "two-statements"
       [VarDeclStmt () [VarDecl () (Id () "x") (Just (NumLit () (Left 10)))],VarDeclStmt () [VarDecl () (Id () "y") (Just (NumLit () (Left 20)))]]
  $: testCase "Switch statement" $$
       parseTest "switch-statements"
       [SwitchStmt () (VarRef () (Id () "foo")) [CaseClause () (NumLit () (Left 10)) [ExprStmt () (CallExpr () (DotRef () (VarRef () (Id () "console")) (Id () "log")) [StringLit () "10!"])],CaseClause () (NumLit () (Left 20)) [ExprStmt () (CallExpr () (DotRef () (VarRef () (Id () "console")) (Id () "log")) [StringLit () "20!"])],CaseDefault () [ExprStmt () (CallExpr () (DotRef () (VarRef () (Id () "console")) (Id () "log")) [StringLit () "something else!"])]],EmptyStmt ()]
  $: testCase "Switch statement w/ two defaults" $$
       expectedParseFail "switch-double-default" (6,12)
  $: testCase "If-statement" $$
       parseTest "if-statement"
       [IfStmt () (PrefixExpr () PrefixLNot (InfixExpr () OpEq (VarRef () (Id () "foo")) (VarRef () (Id () "bar")))) (ExprStmt () (AssignExpr () (VarRef () (Id () "x")) OpAssign (NumLit () (Left 10)))) (IfStmt () (InfixExpr () OpNEq (VarRef () (Id () "foo")) (VarRef () (Id () "bar"))) (BlockStmt () [ExprStmt () (AssignExpr () (VarRef () (Id () "x")) OpAssign (NumLit () (Left 20)))]) (BlockStmt () [ExprStmt () (AssignExpr () (VarRef () (Id () "x")) OpAssign (NumLit () (Left 30)))]))]
  $: testCase "Dangling else" $$
       parseTest "dangling-else"
       [IfStmt () (VarRef () (Id () "foo")) (ExprStmt () (CallExpr () (VarRef () (Id () "bar")) [])) (EmptyStmt ()),IfStmt () (VarRef () (Id () "bar")) (ExprStmt () (CallExpr () (VarRef () (Id () "cux")) [])) (ExprStmt () (CallExpr () (VarRef () (Id () "baz")) []))]
  $: []


pp = putStr . renderStatements
run = defaultMain tests_ecmascript5_parser