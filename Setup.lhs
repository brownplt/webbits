#!/usr/bin/env runhaskell
> import Distribution.Simple
> import qualified Data.List as L
> import System.Directory
> import System.Process (runCommand,waitForProcess)

> isHaskellFile file = ".lhs" `L.isSuffixOf` file || ".hs" `L.isSuffixOf` file

> moduleName file = L.takeWhile (\ch -> ch /= '.') file

> testMain _ _ _ _ = do
>   files <- getDirectoryContents "tests"
>   let tests = filter isHaskellFile files
>   let testModules = map moduleName tests
>   let testFuncs = map (++ ".main") testModules
>   let testExpr = "sequence [ " ++ concat (L.intersperse "," testFuncs) ++ 
>                  " ] >>= \\cases -> runTestTT (TestList cases)"
>   let moduleLine = concat (L.intersperse " " testModules)
>   let cmd = "cd tests && ghc  -XNoMonomorphismRestriction -fglasgow-exts " ++
>             "-package HUnit -i:../src -e \"" ++ testExpr ++ 
>             " >> return ()\" " ++ moduleLine
>   putStrLn "Testing command is:"
>   putStrLn cmd
>   putStrLn "\nLoading tests..."
>   handle <- runCommand cmd
>   waitForProcess handle
>   putStrLn "Testing complete.  Errors reported above (if any)."
 

> main = defaultMainWithHooks (simpleUserHooks { runTests = testMain })
