module Main where

import Language.ECMAScript3.Parser
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Arbitrary()
import Language.ECMAScript3.Syntax.Annotations
import Test.QuickCheck
import System.Exit

main :: IO ()
main = 
  let qcArgs = Args {maxSuccess = 50
                    ,maxDiscardRatio = 10
                    ,maxSize = 10
                    ,replay = Nothing
                    ,chatty = True}
  in quickCheckWithResult qcArgs prettyParseEquivalence >>= \res ->
  case res of
    Success {} -> putStrLn "All tests passes"
    GaveUp {} -> putStrLn "Gave up"
    Failure {} -> putStrLn "Test failed" >> exitFailure
    NoExpectedFailure {} -> putStrLn "Unexpected failure" >> exitFailure
        
prettyParseEquivalence :: JavaScript () -> Bool
prettyParseEquivalence js = 
  case parseScriptFromString "" $ show $ prettyPrint js of
    Left _ -> False
    Right parsed -> (reannotate (const ()) parsed) == js
