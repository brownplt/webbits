module Ovid.Environment
  ( buildJsEnv
  , Env, EnvTree
  , Ann
  , AdditionalAnnotation (..)
  , AnnotatedExpression
  , AnnotatedStatement
  , buildEnvTree
  , makeDynamicEnv
  ) where

--{{{ Imports
import Ovid.Prelude
import Control.Monad.Identity
import Data.Generics hiding (GT)
import Data.Environment hiding (Env)
import qualified Data.Environment as E
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Zipper (ZipperT,TraverserT)
import qualified Data.Map as M
import qualified Data.Zipper as Z
import qualified Data.Foldable as F
import qualified Data.List as L
import Javascript.Javascript
import Text.ParserCombinators.Parsec.Pos (SourcePos,initialPos)
import CFA.Labels
--}}}

--{{{ Environment building types and functions 

type AnnotatedExpression = Expression Ann
type AnnotatedStatement = Statement Ann

data AdditionalAnnotation
  = FnA {
      fnannEnclosing :: Env,
      fnannLocals    :: [Label],
      fnannThisLbl   :: Label 
    }
  | NA
  deriving (Show,Data,Typeable)

type Env = E.Env Label
type EnvTree = Z.Tree Env
type PartialEnv = (Env, [Id SourcePos])
-- type EnvBuilder m a = ZipperT PartialEnv (CounterT m) a
-- type EnvTraverser m a = TraverserT Env (CounterT m) a 

type Ann = (SourcePos,Label,AdditionalAnnotation)

-- addBinding :: Monad m => Id SourcePos -> Label -> EnvBuilder m ()
addBinding id l = do
  (env,ids) <- Z.getNode
  Z.setNode (augmentEnv env id l,ids)

-- newBind :: Monad m => Id SourcePos -> EnvBuilder m ()
newBind id@(Id loc v) = idLabel id >>= addBinding id


-- addRef :: Monad m => Id SourcePos -> EnvBuilder m ()
addRef id = do
  (env,ids) <- Z.getNode
  Z.setNode (env,id:ids)
--}}}
  
--{{{ Step 0 : Add `this' and `arguments' to the formal parameters of all 
-- functions 

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

explicitThis :: [Statement SourcePos] -> [Statement SourcePos]
explicitThis =
  everywhere ((mkT removeParens) . (mkT thisExpr) . (mkT thisStmt))

--}}}

--{{{ Step 1: Build the partial environment

--envId :: Id SourcePos -> EnvBuilder (Id SourcePos)
envId id = return id

--envProp :: Prop SourcePos -> EnvBuilder (Prop SourcePos)
envProp prop = return prop

-- envExpr :: (Monad m) => ParsedExpression -> EnvBuilder m ParsedExpression
envExpr e = 
  case e of
    StringLit _ _ -> return e
    RegexpLit _ _ _ _ -> return e
    NumLit _ _ -> return e
    BoolLit _ _ -> return e
    NullLit _ -> return e
    ArrayLit l es -> liftM (ArrayLit l) (mapM envExpr es)
    ObjectLit l ps ->
      let mpe (p,e) = liftM2 (,) (envProp p) (envExpr e)
        in liftM (ObjectLit l) (mapM mpe ps)
    ThisRef _ -> return e
    VarRef _ id -> addRef id >> return e
    DotRef l e p -> envExpr e >>= \e' -> return (DotRef l e' p)
    BracketRef l e1 e2 -> liftM2 (BracketRef l) (envExpr e1) (envExpr e2)
    NewExpr l e es -> liftM2 (NewExpr l) (envExpr e) (mapM envExpr es)
    PostfixExpr l op e -> liftM (PostfixExpr l op) (envExpr e)
    PrefixExpr l op e -> liftM (PrefixExpr l op) (envExpr e)
    InfixExpr l op e1 e2 -> liftM2  (InfixExpr l op) (envExpr e1) (envExpr e2)
    CondExpr l e1 e2 e3 -> 
      liftM3 (CondExpr l) (envExpr e2) (envExpr e2) (envExpr e3)
    AssignExpr l op (VarRef l' id) e ->
      addRef id >> liftM (AssignExpr l op (VarRef l' id)) (envExpr e)
    AssignExpr l op e1 e2 ->
      liftM2 (AssignExpr l op) (envExpr e1) (envExpr e2)
    ParenExpr l e -> liftM (ParenExpr l) (envExpr e)
    ListExpr l es -> liftM (ListExpr l) (mapM envExpr es)
    -- TODO: Must add references on the LHS of the call.
    CallExpr l e es -> liftM2 (CallExpr l) (envExpr e) (mapM envExpr es)
    FuncExpr l args s -> 
      liftM (FuncExpr l args) (Z.nest (emptyEnv, []) $ mapM_ newBind args >> envStmt s)

envCaseClause (CaseClause l e ss) =
  liftM2 (CaseClause l) (envExpr e) (mapM envStmt ss)
envCaseClause (CaseDefault l ss) =
  liftM (CaseDefault l) (mapM envStmt ss)

envCatchClause (CatchClause l id s) = -- TODO: how is id bound?
  newBind id >> liftM (CatchClause l id) (envStmt s)

envVarDecl (VarDecl l id ye) = do
  newBind id
  liftM (VarDecl l id) (ym envExpr ye)
  
envForInit NoInit = 
  return NoInit
envForInit (VarInit decls) =
  liftM VarInit (mapM envVarDecl decls)
envForInit (ExprInit e) =
  liftM ExprInit (envExpr e)

envForInInit (ForInVar id) =
  newBind id >> return (ForInVar id)
envForInInit (ForInNoVar id) =
  addRef id >> return (ForInNoVar id)

envStmt s =
  case s of
    BlockStmt l ss -> liftM (BlockStmt l) (mapM envStmt ss)
    EmptyStmt _ -> return s
    ExprStmt l e -> liftM (ExprStmt l) (envExpr e)
    IfStmt l e s1 s2 -> liftM3 (IfStmt l) (envExpr e) (envStmt s1) (envStmt s2)
    IfSingleStmt l e s -> liftM2 (IfSingleStmt l) (envExpr e) (envStmt s)
    SwitchStmt l e cs ->  liftM2 (SwitchStmt l) (envExpr e) (mapM envCaseClause cs)
    WhileStmt l e s -> liftM2 (WhileStmt l) (envExpr e) (envStmt s)
    DoWhileStmt l s e -> liftM2 (DoWhileStmt l) (envStmt s) (envExpr e)
    BreakStmt l yid -> liftM (BreakStmt l) (ym envId yid)
    ContinueStmt l yid -> liftM (ContinueStmt l) (envId `ym` yid)
    LabelledStmt l id s -> liftM2 (LabelledStmt l) (envId id) (envStmt s)
    ForInStmt l init e s -> liftM3 (ForInStmt l) (envForInInit init) (envExpr e) (envStmt s)
    ForStmt l init ye1 ye2 s -> 
      liftM4 (ForStmt l) (envForInit init) (ym envExpr ye1) (ym envExpr ye2) (envStmt s)
    TryStmt l s cs ys -> 
      liftM3 (TryStmt l) (envStmt s) (mapM envCatchClause cs) (ym envStmt ys)
    ThrowStmt l e -> liftM (ThrowStmt l) (envExpr e)
    ReturnStmt l ye -> liftM (ReturnStmt l) (ym envExpr ye)
    WithStmt loc e s -> liftM2 (WithStmt loc) (envExpr e) (envStmt s)
    VarDeclStmt loc ds -> liftM (VarDeclStmt loc) (mapM envVarDecl ds)
    FunctionStmt loc id args body -> do 
      newBind id
      body' <- Z.nest (emptyEnv, []) 
                      (do mapM_ newBind args
                          envStmt body)
      return $ FunctionStmt loc id args body'

--}}}
      
--{{{ Step 2 : Transform the partial environment into a complete environment
completeEnvM' :: (UniqueM (t1 m), MonadTrans t1, Monad m) => Z.Tree PartialEnv -> StateT Env (t1 m) (Z.Tree Env)
completeEnvM' pt = Z.dfsFoldM f emptyEnv pt where
  f enclosingEnv (env,refs) = foldM g (joinEnv enclosingEnv env) refs 
  g env id = 
    case lookupEnv env id of
      Nothing  -> do globals <- get
                     case lookupEnv globals id of
                       -- If this global has been declared already, use its
                       -- label.
                       Just lbl -> return (augmentEnv env id lbl)
                       -- If not, create a new label for the global and save it
                       -- to the monadic state so that it is used in the future.
                       Nothing  -> do lbl <- idLabel id
                                      put (augmentEnv globals id lbl)
                                      return (augmentEnv env id lbl)
      Just lbl -> return (augmentEnv env id lbl)

completeEnvM :: (UniqueM (t1 m), MonadTrans t1, Monad m) => Z.Tree PartialEnv -> StateT Env (t1 m) (Z.Tree Env)
completeEnvM pt = do
  tree <- completeEnvM' pt
  globals <- get
  return (fmap (joinEnv globals) tree)
--}}}

--{{{ Step 3 : Annotate the parse tree with environments and label

locOf :: F.Foldable t => t SourcePos -> SourcePos
locOf = fromJust . (F.find $ const True)

--lblId :: (Monad m) => Id SourcePos -> EnvTraverser m (Id Ann)
lblId id@(Id p s) = do
  env <- Z.trGet
  case lookupEnv env id of
    Just lbl -> do
      return (Id (p,lbl,NA) s)
    Nothing  -> fail $ "BUG: Ovid.Environment.lblId: unbound id: " ++ show id ++
                       "at " ++ show p

lblNoVarId id@(Id loc s) = do
 lbl <- locLabel loc
 return $ Id (loc,lbl,NA) s

mkGenericAnnotation t = do
  env <- Z.trGet
  lbl <- locLabel (locOf t)
  return (locOf t,lbl,NA)

-- lblProp :: (Monad m) => Prop SourcePos -> EnvTraverser m (Prop Ann)
lblProp prop = do
  ann <- mkGenericAnnotation prop
  case prop of
    PropId l id    -> do
      liftM (PropId ann) (lblNoVarId id)
    PropString l s -> do
      return $ PropString ann s
    PropNum l n    -> do
      return $ PropNum ann n


lblExpr e = do 
  ann <- mkGenericAnnotation e
  case e of
    StringLit _ s -> return $ StringLit ann s
    RegexpLit _ re g i -> return $ RegexpLit ann re g i
    NumLit _ n -> return $ NumLit ann n
    BoolLit _ b -> return $ BoolLit ann b 
    NullLit _ -> return $ NullLit ann
    ArrayLit l es -> liftM (ArrayLit ann) (mapM lblExpr es)
    ObjectLit l ps ->
      let mpe (p,e) = liftM2 (,) (lblProp p) (lblExpr e)
        in liftM (ObjectLit ann) (mapM mpe ps)
    ThisRef loc -> do
      env <- Z.trGet
      thisLbl <- lookupEnv env (Id () "this")
      return $ ThisRef (loc,thisLbl,NA)
    VarRef loc id@(Id p v) -> do
      env <- Z.trGet
      case lookupEnv env id of
        Just idLbl -> do
          liftM (VarRef (loc,idLbl,NA)) (lblId id)
        Nothing  -> fail $ "(bug) Ovid.lblExpr: unbound identifier " ++ show v 
                           ++ " at " ++show p
    DotRef l e id -> liftM2 (DotRef ann) (lblExpr e) (lblNoVarId id)
    BracketRef l e1 e2 -> liftM2 (BracketRef ann) (lblExpr e1) (lblExpr e2)
    NewExpr l e es -> liftM2 (NewExpr ann) (lblExpr e) (mapM lblExpr es)
    PostfixExpr l op e -> liftM (PostfixExpr ann op) (lblExpr e)
    PrefixExpr l op e -> liftM (PrefixExpr ann op) (lblExpr e)
    InfixExpr l op e1 e2 -> liftM2  (InfixExpr ann op) (lblExpr e1) (lblExpr e2)
    CondExpr l e1 e2 e3 -> 
      liftM3 (CondExpr ann) (lblExpr e2) (lblExpr e2) (lblExpr e3)
    AssignExpr l op e1 e2 ->
      liftM2 (AssignExpr ann op) (lblExpr e1) (lblExpr e2)
    ParenExpr l e -> liftM (ParenExpr ann) (lblExpr e)
    ListExpr l es -> liftM (ListExpr ann) (mapM lblExpr es)
    CallExpr l e es -> liftM2 (CallExpr ann) (lblExpr e) (mapM lblExpr es)
    FuncExpr loc args s -> do
      enclosingEnv <- Z.trGet
      fnLabel <- locLabel loc
      localEnv <- Z.trDown Z.trGet
      thisLbl <- lookupEnv localEnv (Id () "this")
      args' <- Z.trDown (mapM lblId args)
      s' <- Z.trDown (lblStmt s)
      Z.trCanGoRight >>= (flip when Z.trRight)
      let localsAndArgs =
            map snd $ M.toList (internalMap (E.diffEnv localEnv enclosingEnv))
      let idLabel (Id (_,lbl,_) _) = lbl
      let argLabels = map idLabel args'
      let locals = foldr L.delete localsAndArgs argLabels
      return (FuncExpr (loc,fnLabel,FnA enclosingEnv locals thisLbl) args' s')

                            
lblCaseClause cc@(CaseClause l e ss) = do
  ann <- mkGenericAnnotation cc
  liftM2 (CaseClause ann) (lblExpr e) (mapM lblStmt ss)
lblCaseClause cc@(CaseDefault l ss) = do
  ann <- mkGenericAnnotation cc
  liftM (CaseDefault ann) (mapM lblStmt ss)

lblCatchClause e@(CatchClause l id s) = do -- TODO: how is id bound?
  ann <- mkGenericAnnotation e
  liftM2 (CatchClause ann) (lblId id) (lblStmt s)

lblVarDecl (VarDecl loc id ye) = do
  env <- Z.trGet
  case lookupEnv env id of
    Nothing  -> 
      fail $ "lblVarDecl : unbound identifier: " ++ show id ++ " (program bug)"
    Just lbl -> liftM2 (VarDecl (loc,lbl,NA)) (lblId id) (ym lblExpr ye)
  
lblForInit NoInit = 
  return NoInit
lblForInit (VarInit decls) =
  liftM VarInit (mapM lblVarDecl decls)
lblForInit (ExprInit e) =
  liftM ExprInit (lblExpr e)

lblForInInit (ForInVar id) =
  liftM ForInVar (lblId id)
lblForInInit (ForInNoVar id) =
  liftM ForInNoVar (lblId id)

lblStmt s = do
  ann <- mkGenericAnnotation s
  case s of
    BlockStmt _ ss -> liftM (BlockStmt ann) (mapM lblStmt ss)
    EmptyStmt _    -> return $ EmptyStmt ann
    ExprStmt _ e   -> liftM (ExprStmt ann) (lblExpr e)
    IfStmt _ e s1 s2   -> liftM3 (IfStmt ann) (lblExpr e) (lblStmt s1) (lblStmt s2)
    IfSingleStmt _ e s -> liftM2 (IfSingleStmt ann) (lblExpr e) (lblStmt s)
    SwitchStmt _ e cs ->  liftM2 (SwitchStmt ann) (lblExpr e) (mapM lblCaseClause cs)
    WhileStmt _ e s -> liftM2 (WhileStmt ann) (lblExpr e) (lblStmt s)
    DoWhileStmt _ s e -> liftM2 (DoWhileStmt ann) (lblStmt s) (lblExpr e)
    BreakStmt _ yid -> liftM (BreakStmt ann) (ym lblNoVarId yid)
    ContinueStmt _ yid -> liftM (ContinueStmt ann) (lblNoVarId `ym` yid)
    LabelledStmt _ id s -> liftM2 (LabelledStmt ann) (lblNoVarId id) (lblStmt s)
    ForInStmt _ init e s -> liftM3 (ForInStmt ann) (lblForInInit init) (lblExpr e) (lblStmt s)
    ForStmt _ init ye1 ye2 s -> 
      liftM4 (ForStmt ann) (lblForInit init) (ym lblExpr ye1) (ym lblExpr ye2) (lblStmt s)
    TryStmt _ s cs ys -> 
      liftM3 (TryStmt ann) (lblStmt s) (mapM lblCatchClause cs) (ym lblStmt ys)
    ThrowStmt _ e -> liftM (ThrowStmt ann) (lblExpr e)
    ReturnStmt _ ye -> liftM (ReturnStmt ann) (ym lblExpr ye)
    WithStmt _ e s -> liftM2 (WithStmt ann) (lblExpr e) (lblStmt s)
    VarDeclStmt _ ds -> liftM (VarDeclStmt ann) (mapM lblVarDecl ds)
    FunctionStmt loc id@(Id idLoc sId) args s -> do
      enclosingEnv <- Z.trGet
      fnLabel <- lookupEnv enclosingEnv id
      localEnv <- Z.trDown Z.trGet
      thisLbl <- lookupEnv localEnv (Id () "this")
      (s',args') <- Z.trDown (liftM2 (,) (lblStmt s) (mapM lblId args))
      Z.trCanGoRight >>= (flip when Z.trRight)
      let localsAndArgs =
            map snd $ M.toList (internalMap (E.diffEnv localEnv enclosingEnv))
          idLabel (Id (_,lbl,_) _) = lbl
          argLabels = map idLabel args'
          locals = foldr L.delete localsAndArgs argLabels
      return (FunctionStmt (loc,fnLabel,
                            FnA enclosingEnv locals thisLbl)
                           (Id (idLoc,fnLabel,NA) sId) args' s')

--}}}

--{{{ Put it all together

globalPartialEnv lbl = 
  Z.toLocation (Z.Node (E.singletonEnv (Id () "this") lbl,[]) [])

-- addTopLevelIds :: Monad m => [String] -> EnvBuilder m ()
addTopLevelIds topLevelIds = do
  mapM_ (\id -> builtinLabel id >>= addBinding (Id (initialPos "top-level") id))
        topLevelIds

buildEnvTree ::  (UniqueM (t1 m), MonadTrans t1, Monad m) 
             => [String]
             -> [Statement SourcePos] 
             -> t1 m (Z.Tree Env)
buildEnvTree topLevelIds ss = do
  thisLbl <- newLabel' -- this seems a little fishy
  partialEnvTree <- Z.execZipperT (addTopLevelIds topLevelIds >> mapM envStmt ss)
                                  (globalPartialEnv thisLbl)
  (v,s) <- runStateT (completeEnvM partialEnvTree) E.emptyEnv
  return v

  
  
makeDynamicEnv :: (UniqueM (t1 m), MonadTrans t1, Monad m)
               => [Label] -- ^ existing top-level names
               -> [Statement SourcePos]
               -> t1 m ([Label],[AnnotatedStatement]) -- ^ returns a list of top-level names
makeDynamicEnv globalsAndThis parsedStmts = do
  let stmts = explicitThis parsedStmts -- make 'this' and 'arguments' explicit (and maybe other things)
  let ([thisLbl],globals) = partition (\lbl -> unsafeLabelIx lbl == Just 0) globalsAndThis
  let addGlobal lbl = case labelName lbl of
        Just id -> do
          (env,ids) <- Z.getNode
          Z.setNode (augmentEnv env (Id (initialPos "top-level") id) lbl,ids)
        otherwise -> fail $ "makeDynamicEnv: expected global to have a name -- " ++ show lbl
  partialEnvTree <- Z.execZipperT (mapM_ addGlobal globals >> mapM envStmt stmts) (globalPartialEnv thisLbl) 
  (envTree,_) <- runStateT (completeEnvM partialEnvTree) E.emptyEnv
  stmts <- Z.evalTraverserT (mapM lblStmt stmts) envTree
  let globals' = map snd $ M.toList (E.internalMap $ Z.rootLabel envTree)
  return (globals',stmts) 

buildJsEnv :: (UniqueM (t1 m), MonadTrans t1, Monad m) 
           => [String] 
           -> [Statement SourcePos] 
           -> t1 m (Z.Tree Env,[Statement Ann])
buildJsEnv topLevelIds ss = do
  let ss' = explicitThis ss
  envTree  <- buildEnvTree topLevelIds ss'
  ss'' <- Z.evalTraverserT (mapM lblStmt ss')  envTree
  return (envTree,ss'')
  
--}}}
