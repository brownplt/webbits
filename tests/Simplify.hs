-- |Runs the WebKit-Mozilla test suite with Rhino.  This does not test WebBits.
module Simplify where

import qualified Data.List as L
import System.FilePath
import System.Directory
import Control.Monad

import WebBits.Test
import WebBits.JavaScript
import WebBits.JavaScript.Simplify
import qualified Data.ByteString.Char8 as B
  
suiteDir = "webkit-moz-suite"

isTestCall :: Statement SourcePos -> Bool
isTestCall (ExprStmt _ (ListExpr _ [CallExpr _ (VarRef _ (Id _ "test")) _])) 
  = True
isTestCall _ = False

data MozillaTestCase
  = MozillaTestCase FilePath -- ^support file
                    FilePath -- ^test file
                    [Statement SourcePos] -- ^support file statements
                    [Statement SourcePos] -- ^statements before test(..)
                    [Statement SourcePos] -- ^statements after test(..)
                    (Statement SourcePos) -- ^the test(..) statement
  | InvalidMozillaTestCase FilePath -- ^test file
                           String -- ^details

prepareMozillaTestCase :: FilePath -- ^support file
                       -> FilePath -- ^test file
                       -> IO MozillaTestCase
prepareMozillaTestCase supportFile testFile = do
  supportStmts <- parseJavaScriptFromFile supportFile
  suiteStmts <- parseJavaScriptFromFile testFile
  let (testStmts,testSupportStmts) = L.break isTestCall suiteStmts
  case testSupportStmts of
    (testStmt:testSupportStmts') -> 
      return $ MozillaTestCase supportFile testFile supportStmts suiteStmts
                               testSupportStmts' testStmt
    otherwise -> 
      return $ InvalidMozillaTestCase testFile "could not find test(..)"

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
                              -> IO [(FilePath,FilePath)]
getMozillaTestCasesInCategory category = do
  files <- allFiles category
  let jsFiles = filter (\f -> takeExtension f == ".js") files
  case jsFiles of
    (shell_js:files) -> return $ map ((,)shell_js) files
    otherwise -> return []
  
-- |Returns a list of ([support-file, ...],test-file)
getMozillaTestCases :: IO [(FilePath,FilePath)]
getMozillaTestCases = do
  categories' <- getDirectoryContents suiteDir
  let categories = filter isInterestingPath categories'
  liftM concat $ mapM (getMozillaTestCasesInCategory.(suiteDir</>)) categories



mozillaTestCase :: MozillaTestCase -> Test
mozillaTestCase (InvalidMozillaTestCase path "") = TestCase $
  assertFailure $ "unknown error in " ++ path
mozillaTestCase (InvalidMozillaTestCase path msg) = TestCase $
  assertFailure $ "error in " ++ path ++ ":\n" ++ msg
mozillaTestCase (MozillaTestCase supportPath testPath 
                                 prefixStmts testStmts 
                                 suffixStmts testCallStmt) = TestCase $ do
  let stmts = prefixStmts ++ suffixStmts ++ simplify testStmts ++ [testCallStmt]
  r <- rhinoIOFile (B.pack $ pretty stmts)
  case r of
    Right _ -> return ()
    Left stderr -> assertFailure $ 
      "failed on " ++ testPath ++ "\n" ++
      "stderr is:\n" ++                       
      (B.unpack stderr)

single support test = do
  tc <- prepareMozillaTestCase (suiteDir</>support) (suiteDir</>test)
  runTestTT $ mozillaTestCase tc


main = do
  testCaseFiles <- getMozillaTestCases
  testCaseData <- mapM (uncurry prepareMozillaTestCase) testCaseFiles
  return $ TestList $ map mozillaTestCase testCaseData

