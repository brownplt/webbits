module Main where

import Test.HUnit
import System.Exit
import System.Directory
import qualified System.FilePath as FilePath
import Language.ECMAScript3.Parser
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax

testDir = "tests/parse-pretty"

parsePrettyTest filename = TestLabel filename $ TestCase $ do
  js <- parseJavaScriptFromFile filename
  let str = renderStatements js
  case parseScriptFromString "" str of
    Left err -> assertFailure (show err)
    Right (Script _ js') -> do
      let str' = renderStatements js'
      assertBool "pretty-printed code should re-parse" (str == str')

main = do
  allFiles <- getDirectoryContents testDir
  let files =  map (testDir `FilePath.combine`) $ 
        filter (\x -> FilePath.takeExtension x == ".js") allFiles
  let parsePretty = TestLabel "parser - printer composition" 
        (TestList (map parsePrettyTest files))
  results <- runTestTT parsePretty
  if errors results > 0 || failures results > 0 
    then exitFailure
    else putStrLn "All tests passed."
