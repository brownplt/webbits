module Main where

import Language.ECMAScript3.Parser
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Arbitrary()
import Language.ECMAScript3.Syntax.Annotations
import System.Exit
import Test.SourceDiff
import Test.QuickCheck

main :: IO ()
main = 
  let qcArgs = Args {maxSuccess = 50
                    ,maxDiscardRatio = 10
                    ,maxSize = 10
                    ,replay = Nothing
                    ,chatty = False}
  in quickCheckWithResult qcArgs prettyParseEquivalence >>= \res ->
  case res of
    Success {} -> putStrLn "All tests passes"
    GaveUp {} -> putStrLn "Gave up"
    Failure {} -> putStrLn "Test failed" >> exitFailure
    NoExpectedFailure {} -> putStrLn "Unexpected failure" >> exitFailure

prettyParseEquivalence :: JavaScript () -> Property
prettyParseEquivalence js =
  let pp = show $ prettyPrint js
  in case parseScriptFromString "" pp of
    Left e -> 
      let err = "Can't parse pretty-printed code. The error was: " ++ (show e) ++
                "\nThe pretty-printed code in question:\n" ++ pp
      in whenFail (putStrLn err) False
    Right parsed ->
      let eq = (removeAnnotations parsed) == js
          msg ="The parse of the pretty-printed AST didn't match the original\n"
               ++"Diff:\n" ++ jsDiff js (reannotate (const ()) parsed)
      in whenFail (putStrLn msg) eq
