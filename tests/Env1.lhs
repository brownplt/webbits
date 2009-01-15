var x = 12;

(function (x) { return x; })(x);
  

/*

> module Env1 where

> import Test.HUnit
> import WebBits.Test

> test1 = TestCase $ do
>   parsedStmts <- parseJavaScriptFromFile "Env1.lhs"
>   let stmts = env parsedStmts
>   sameIds [(3,12),(3,24)] stmts

> main :: IO Test
> main = do
>   return $ TestList [test1]

*/
