function foo() {
  return function () {
    return x;
  }
  var x;
}

/*

> module Test.EnvTest3 where

> import Test.Util.Environment

> main = do
>   stmts <- testFile "Test/EnvTest3.lhs"
>   return $ TestList
>     [ testLabelEq stmts (3,12) (5,7)
>     ]

*/
