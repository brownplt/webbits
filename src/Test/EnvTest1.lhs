function foo(x) {
  return x;
}

/*

> module Test.EnvTest1 where

> import Test.Util.Environment

> main = do
>   stmts <- testFile "Test/EnvTest1.lhs"
>   return $ testLabelEq stmts  (1,14) (2,10)

*/
