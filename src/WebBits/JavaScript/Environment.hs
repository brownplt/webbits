module WebBits.JavaScript.Environment 
  ( staticEnvironment
  , Ann
  , LabelledStatement
  , LabelledExpression
  , Env
  ) where

import Data.Generics hiding (GT)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import WebBits.Data.Zipper (ZipperT,ZipperT)
import qualified Data.Map as M
import qualified WebBits.Data.Zipper as Z
import Control.Monad.State
import qualified Data.Foldable as F
import qualified Data.List as L
import WebBits.JavaScript.JavaScript
import Text.ParserCombinators.Parsec.Pos (SourcePos,initialPos)

---
-- Add `this' and `arguments' to the formal parameters of all 
-- functions 
--
-- TODO: Does a local var x shadow a formal argument x?  What about 'arguments'?

thisStmt :: Statement SourcePos -> Statement SourcePos
thisStmt (FunctionStmt loc id args body) = 
  FunctionStmt loc id ((Id loc "this"):(Id loc "arguments"):args) body
thisStmt s = s

thisExpr :: Expression SourcePos -> Expression SourcePos
thisExpr (FuncExpr l args s) = 
  FuncExpr l ((Id l "this"):(Id l "arguments"):args) s
thisExpr e = e

removeParens :: Expression SourcePos -> Expression SourcePos
removeParens (ParenExpr _ e) = e
removeParens e = e

removeSingletons :: Expression SourcePos -> Expression SourcePos
removeSingletons (ListExpr _ [e]) = e
removeSingletons e = e

explicitThis :: [Statement SourcePos] -> [Statement SourcePos]
explicitThis = everywhere $ (mkT removeSingletons) . (mkT removeParens) 
  . (mkT thisExpr) . (mkT thisStmt)

-- JavaScript has a global and function scopes.  Globals do not need to be
-- declared.  Any "unbound identifier" in a function is treated as a reference
-- to a global.

-- For each FunctionExpr and FunctionStmt, we create a list of locally defined
-- identifiers (var x = ...) and free identifiers.

-- We build a tree of "partial environments."  Since a variable may be used
-- before it is declared locally, we maintain a set of free identifiers and
-- narrow the set when appropriate.
type PartialEnv = (Env,S.Set String)

type Env = M.Map String Int

emptyPartialEnv :: PartialEnv
emptyPartialEnv = (M.empty,S.empty)

type RefM a = Z.ZipperT PartialEnv (State Int) a

nextLabel :: (MonadTrans t, Monad (t (State Int))) => t (State Int) Int
nextLabel = do
  l <- lift get
  lift $ modify (+1)
  return l 
  
bind :: Id SourcePos -> RefM ()
bind (Id _ s) = do
  lbl <- nextLabel
  (env,freeIds) <- Z.getNode
  Z.setNode (M.insert s lbl env, S.delete s freeIds)
  
use :: Id SourcePos -> RefM ()
use (Id _ s) = do
  (env,freeIds) <- Z.getNode
  case M.lookup s env of
    Just _ -> return ()
    Nothing -> Z.setNode (env, S.insert s freeIds)
  

buildExpr :: Expression SourcePos -> RefM (Expression SourcePos)
buildExpr (FuncExpr loc args stmt) = do
  stmt' <- Z.nest emptyPartialEnv (mapM_ bind args >> buildStmt stmt)
  return (FuncExpr loc args stmt')
buildExpr e@(VarRef _ id) = do
  use id
  return e
buildExpr (AssignExpr loc op (VarRef loc' id) e) = do
  use id
  e' <- buildExpr e
  return (AssignExpr loc op (VarRef loc' id) e')
buildExpr e =
  gmapM buildAny e

buildCatchClause :: CatchClause SourcePos -> RefM (CatchClause SourcePos)
buildCatchClause (CatchClause loc id stmt) = do
  bind id
  stmt' <- buildStmt stmt
  return (CatchClause loc id stmt')
  
buildVarDecl :: VarDecl SourcePos -> RefM (VarDecl SourcePos)
buildVarDecl (VarDecl loc id ye) = do
  bind id
  ye' <- gmapM buildAny ye
  return (VarDecl loc id ye')

buildForInInit :: ForInInit SourcePos -> RefM (ForInInit SourcePos)
buildForInInit e@(ForInVar id) = do
  bind id
  return e
buildForInInit e@(ForInNoVar id) = do
  use id
  return e
  
buildStmt :: Statement SourcePos -> RefM (Statement SourcePos)
buildStmt (FunctionStmt loc id args stmt) = do
  stmt' <- Z.nest emptyPartialEnv (mapM_ bind args >> buildStmt stmt)
  return (FunctionStmt loc id args stmt')
buildStmt s = gmapM buildAny s

buildAny' :: (Data a, Typeable a) => a -> RefM a
buildAny' v = gmapM buildAny v

buildAny :: GenericM RefM
buildAny =
  buildAny' `extM` buildExpr `extM` buildCatchClause `extM`
    buildVarDecl `extM` buildForInInit `extM` buildStmt

--
-- buildAny creates a tree of partial environments.  We now walk the tree and
-- attempt to associate free identifiers with their bindings in enclosing
-- scopes.  Any remaining free identifiers are globals.
--

resolveFreeId :: Env -> String -> StateT Env (State Int) Env
resolveFreeId env freeId = case M.lookup freeId env of
  Just lbl -> return env -- the free identifier was bound in an enclosing env
  Nothing  -> do -- this is a global 
    globals <- get
    case M.lookup freeId globals of
      Just lbl -> return (M.insert freeId lbl env) -- global already bound
      Nothing -> do -- new global
        lbl <- nextLabel
        put (M.insert freeId lbl globals)
        return (M.insert freeId lbl env)


completeEnvM' :: Z.Tree PartialEnv -> StateT Env (State Int) (Z.Tree Env)
completeEnvM' pt = Z.dfsFoldM f M.empty pt where
  -- the union is left-biased; bindings in env shadow bindings in enclosingEnv
  f enclosingEnv (env,freeIds) = 
    foldM resolveFreeId (M.union env enclosingEnv) (S.elems freeIds) 

completeEnvM :: Z.Tree PartialEnv -> StateT Env (State Int) (Z.Tree Env)
completeEnvM pt = do
  tree <- completeEnvM' pt
  globals <- get
  -- left-biased union: lexicals shadow globals
  return (fmap (\lexicals -> M.union lexicals globals) tree)

--
-- completeEnvM creates a tree of environments whose structure is identical to
-- the function-nesting structure of the JavaScript source.  We walk the tree
-- and the code in step and annotate the code with environments.
--

type Ann = (Env,Int,SourcePos)

type LabelledStatement = Statement Ann
type LabelledExpression = Expression Ann

-- Necessary for type-checking.  gmapM won't let us transform the type of the
-- annotation.  So, we first inject SourcePos into a trivial Ann.
insertEmptyAnn :: (Functor f) => f SourcePos -> f Ann
insertEmptyAnn = fmap (\loc -> (M.empty,0,loc))


locOf :: F.Foldable t => t SourcePos -> SourcePos
locOf = fromJust . (F.find $ const True)

labelEnv :: Env -> Z.ZipperT Env (State Int) Env
labelEnv env = Z.getNode

labelId :: Id Ann -> Z.ZipperT Env (State Int) (Id Ann)
labelId id@(Id (_,_,loc) s) = do
  env <- Z.getNode
  case M.lookup s env of
    Nothing -> fail $ "BUG: unbound identifier while labelling" ++ show id
    Just lbl -> return (Id (env,lbl,loc) s)
    
labelIdNoVar :: Id Ann -> Z.ZipperT Env (State Int) (Id Ann)
labelIdNoVar (Id (_,_,loc) s) = do
  env <- Z.getNode
  lbl <- nextLabel
  return (Id (env,lbl,loc) s)

labelProp :: Prop Ann -> Z.ZipperT Env (State Int) (Prop Ann)
labelProp (PropId (_,_,loc) id) = do
  env <- Z.getNode
  lbl <- nextLabel
  id' <- labelIdNoVar id
  return (PropId (env,lbl,loc) id')
labelProp e = gmapM labelAny e



labelExpr :: Expression Ann 
          -> Z.ZipperT Env (State Int) (Expression Ann)
labelExpr (ThisRef (_,_,loc)) = do
  env <- Z.getNode
  case M.lookup "this" env of
    Just lbl -> return $ ThisRef (env,lbl,loc)
    Nothing -> fail "BUG: expected to find this in the environment"
labelExpr (DotRef (_,_,loc) expr id) = do
  env <- Z.getNode
  lbl <- nextLabel
  id' <- labelIdNoVar id
  expr' <- labelExpr expr
  return (DotRef (env,lbl,loc) expr' id')
labelExpr (FuncExpr (_,_,loc) args stmt) = do
  env <- Z.getNode
  lbl <- nextLabel
  args' <- Z.withCurrentChild (mapM labelId args)
  stmt' <- Z.withCurrentChild (labelStmt stmt)
  Z.shiftRight'
  return (FuncExpr (env,lbl,loc) args' stmt')
labelExpr e = gmapM labelAny e
  
labelStmt :: Statement Ann
          -> Z.ZipperT Env (State Int) (Statement Ann)
labelStmt (BreakStmt (_,_,loc) (Just id)) = do
  env <- Z.getNode
  lbl <- nextLabel
  id' <- labelIdNoVar id
  return (BreakStmt (env,lbl,loc) (Just id'))
labelStmt (ContinueStmt (_,_,loc) (Just id)) = do
  env <- Z.getNode
  lbl <- nextLabel
  id' <- labelIdNoVar id
  return (ContinueStmt (env,lbl,loc) (Just id'))
labelStmt (LabelledStmt (_,_,loc) id stmt) = do
  env <- Z.getNode
  lbl <- nextLabel
  id' <- labelIdNoVar id
  stmt' <- labelStmt stmt
  return (LabelledStmt (env,lbl,loc) id' stmt')  
labelStmt (FunctionStmt (_,_,loc) id args stmt) = do
  env <- Z.getNode
  lbl <- nextLabel
  args' <- Z.withCurrentChild (mapM labelId args)
  stmt' <- Z.withCurrentChild (labelStmt stmt)
  Z.shiftRight'
  return (FunctionStmt (env,lbl,loc) id args' stmt')
labelStmt e = gmapM labelAny e

labelAny' :: (Data a, Typeable a) => a -> Z.ZipperT Env (State Int) a
labelAny' a = gmapM labelAny a

labelAny :: GenericM (Z.ZipperT Env (State Int)) 
labelAny a = (labelAny' `extM` labelEnv `extM` labelId `extM` labelProp `extM`
  labelExpr `extM` labelStmt) a

-- |Annotates each expression with its static environment.  In addition,
-- a map of free identifiers is returned, along with the next valid label.
staticEnvironment :: [Statement SourcePos] 
                  -> ([Statement Ann],Env,Int)
staticEnvironment stmts =
  let stmts' = explicitThis stmts
      labelM = do
        partialEnvTree <- Z.execZipperT (mapM buildStmt stmts')
                            (Z.toLocation (Z.Node emptyPartialEnv []))
        (envTree,globals) <- runStateT (completeEnvM partialEnvTree) M.empty
        let stmts'' = map insertEmptyAnn stmts' 
        labelledStmts <- Z.evalZipperT (mapM labelStmt stmts'') 
                                       (Z.toLocation envTree)
        return (labelledStmts,globals)
      ((labelledStmts,globals),nextLabel) = runState labelM 0
    in (labelledStmts,globals,nextLabel)
