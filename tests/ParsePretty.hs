module ParsePretty where

import Test.HUnit.Base
import Test.HUnit.Text
import Data.Data
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, FilePath, (</>))

import WebBits.Common (pp)
import WebBits.JavaScript.PrettyPrint () -- instances only
import WebBits.JavaScript.Syntax (JavaScript (..))
import WebBits.JavaScript.Parser (parseScriptFromString, ParsedStatement)
import Text.PrettyPrint.HughesPJ (render, vcat)


pretty :: [ParsedStatement] -> String
pretty stmts = render $ vcat $ map pp stmts

parse :: FilePath -> String -> [ParsedStatement]
parse src str = case parseScriptFromString src str of
  Left err -> error (show err)
  Right (Script _ stmts) -> stmts

eraseSourcePos x = fmap (\_ -> ()) x

assertEqualWithoutSourcePos lhs rhs = 
  case eraseSourcePos lhs == eraseSourcePos rhs of
    True -> return ()
    False -> assertFailure $ "expected:\n" ++ show lhs ++ "\ngot:\n" ++
                             show rhs ++ "\n"

testParsePrettyIdentity :: FilePath -> String -> Test
testParsePrettyIdentity src str = TestCase $ 
  assertEqualWithoutSourcePos lhs rhs where
    lhs = ((parse (src ++ " (PRETTY-PRINTING)")) . pretty . (parse src)) str
    rhs = parse src str
  

main = do
  allPaths <- getDirectoryContents "parse-pretty"
  let testPaths = map ("parse-pretty"</>)
                    $ filter ((== ".js") . takeExtension) allPaths
  testData <- mapM readFile testPaths
  let tests = zipWith testParsePrettyIdentity testPaths testData
  return (TestList tests)
