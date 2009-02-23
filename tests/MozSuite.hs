-- |Runs the WebKit-Mozilla test suite with Rhino.  This does not test WebBits.
module MozSuite where

import qualified Data.List as L
import System.FilePath
import System.Directory
import Control.Monad

import WebBits.Test
import WebBits.JavaScript

isTestCall :: Statement SourcePos -> Bool
isTestCall (ExprStmt _ (ListExpr _ [CallExpr _ (VarRef _ (Id _ "test")) _])) 
  = True
isTestCall _ = False

prepareMozillaTestCase :: [FilePath] -- ^support files, in order
                       -> FilePath -- ^test file
                       -- |Returns statements from the support file, from the
                       -- test file, upto the call to test(..) and statements
                       -- after the call to test(..).
                       -- If a test(..) call is found, it is returned as the
                       -- fourth result.
                       -> IO ([Statement SourcePos],
                              [Statement SourcePos],
                              [Statement SourcePos],
                              Maybe (Statement SourcePos))
prepareMozillaTestCase supportFiles testFile = do
  supportStmts <- liftM concat (mapM parseJavaScriptFromFile supportFiles) 
  suiteStmts <- parseJavaScriptFromFile testFile
  let (testStmts,testSupportStmts) = L.break isTestCall suiteStmts
  case testSupportStmts of
    (testStmt:testSupportStmts') -> 
      return (supportStmts,testStmts,testSupportStmts',Just testStmt)
    otherwise ->
      return (supportStmts,testStmts,testSupportStmts,Nothing)

getMozillaTestCasesInCategory :: FilePath
                              -> [([FilePath],FilePath)]
getMozillaTestCasesInCategory = return undefined

getMozillaTestCases :: IO [([FilePath],FilePath)]
getMozillaTestCases = do
  categories <- getDirectoryContents "webkit-moz-suite"
  return $ liftM concat (mapM getMozillaTestCasesInCategory categories)

main = return ()

