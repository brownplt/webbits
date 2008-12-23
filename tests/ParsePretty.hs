module ParsePretty where

import Test.HUnit.Base
import Test.HUnit.Text
import Data.Data
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, FilePath, (</>))
import qualified Data.List as L
import System.IO.Unsafe (unsafePerformIO)

import WebBits.Common (pp)
import Text.ParserCombinators.Parsec (ParseError, sourceName, errorPos)
import WebBits.JavaScript.PrettyPrint () -- instances only
import WebBits.JavaScript.Syntax (JavaScript (..))
import WebBits.JavaScript.Parser (parseScriptFromString, ParsedStatement)
import Text.PrettyPrint.HughesPJ (render, vcat)


pretty :: [ParsedStatement] -> String
pretty stmts = render $ vcat $ map pp stmts

isPrettyPrintError :: ParseError -> Bool
isPrettyPrintError pe = 
  "(PRETTY-PRINTING)" `L.isSuffixOf` sourceName (errorPos pe)

parse :: FilePath -> String -> [ParsedStatement]
parse src str = case parseScriptFromString src str of
  Left err | isPrettyPrintError err -> 
               (unsafePerformIO $ putStrLn str) `seq` error (show err)
           | otherwise -> error (show err)
  Right (Script _ stmts) -> stmts

eraseSourcePos x = fmap (const ()) x

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
