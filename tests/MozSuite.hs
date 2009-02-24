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
isInterestingPath p = case takeFileName p of
  "."  -> False
  ".." -> False
  otherwise -> True

-- |Returns all files in 'path'.  The returned paths are of the form
-- 'path</>subpath'.
allFiles :: FilePath -> IO [FilePath]
allFiles path = do
  subPaths' <- getDirectoryContents path
  let subPaths = map (path</>) (filter isInterestingPath subPaths')
  -- Normally, not.doesFileExist => doesDirectoryExist.  However, all is not
  -- normal.
  files <- filterM doesFileExist subPaths
  subdirs <- filterM doesDirectoryExist subPaths
  files' <- liftM concat $ mapM allFiles subdirs
  -- Since each of subdirs is path</>subpath, there is no need to preprend
  -- path</> 
  return (files ++ files')

getMozillaTestCasesInCategory :: FilePath
                              -> IO [([FilePath],FilePath)]
getMozillaTestCasesInCategory category = do
  files <- allFiles category
  let jsFiles = filter (\f -> takeExtension f == ".js") files
  case jsFiles of
    (shell_js:files) -> return $ map ((,)[shell_js]) files
    otherwise -> return []
  
-- |Returns a list of ([support-file, ...],test-file)
getMozillaTestCases :: IO [([FilePath],FilePath)]
getMozillaTestCases = do
  let suiteDir = "webkit-moz-suite"
  categories' <- getDirectoryContents suiteDir
  let categories = filter isInterestingPath categories'
  liftM concat $ mapM (getMozillaTestCasesInCategory.(suiteDir</>)) categories

prepareMozillaTestCases :: IO [([Statement SourcePos],
                                [Statement SourcePos],
                                [Statement SourcePos],
                                Maybe (Statement SourcePos))]
prepareMozillaTestCases = do
  cases <- getMozillaTestCases
  mapM (uncurry prepareMozillaTestCase) cases

main :: IO ()
main = return ()

