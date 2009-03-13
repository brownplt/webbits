module WebBits.JavaScript.Simplify
  ( simplify
  ) where

import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Data.List as L
import Control.Monad.State

import WebBits.Common
import Data.Generics

import WebBits.JavaScript.Syntax
import qualified WebBits.JavaScript.Core as Core
import WebBits.JavaScript.Env

noPos = initialPos "WebBits.JavaScript.CoreTransform"

removeFuncStmt :: Statement SourcePos -> Statement SourcePos
removeFuncStmt (FunctionStmt p name args body) = VarDeclStmt p [decl] where
  decl = VarDecl p name (Just funcExpr)
  funcExpr = FuncExpr p args body
removeFuncStmt stmt = stmt

-- |For each FuncExpr, creates a VarDecl at the top that names all locally
-- defined variables.
pseudoLetBindings :: Expression SourcePos -> Expression SourcePos
pseudoLetBindings (FuncExpr p args body) = FuncExpr p args body' where
  body' = BlockStmt p [binds,body]
  binds = VarDeclStmt p (map (\name -> VarDecl p (Id p name) Nothing) locals)
  locals = S.toList (localVars [body])
pseudoLetBindings expr = expr
  
removeVarDecl :: Statement SourcePos -> Statement SourcePos
removeVarDecl (VarDeclStmt p decls) = case Y.mapMaybe unDecl decls of
  [] -> EmptyStmt p
  exprs -> BlockStmt p $ map (ExprStmt p) exprs
removeVarDecl stmt                  = stmt

unDecl :: VarDecl SourcePos -> Maybe (Expression SourcePos)
unDecl (VarDecl p id Nothing) = Nothing
unDecl (VarDecl p id (Just e)) = Just $ AssignExpr p OpAssign (VarRef p id) e 


-- |Once all variables are declared at the head of each function, we can
-- remove variable declarations from within the function body.
removeInnerVarDecls :: Expression SourcePos -> Expression SourcePos
removeInnerVarDecls (FuncExpr p args (BlockStmt p' [binds,body])) = expr' where
  expr' = FuncExpr p args (BlockStmt p' [binds,body'])
  body' = everywhereBut (not.excludeFunctions) 
            (mkT removeVarDecl `extT` unVarForInit `extT` unVarForInInit)
            body
 

  unVarForInit :: ForInit SourcePos -> ForInit SourcePos
  unVarForInit (VarInit decls) = case Y.mapMaybe unDecl decls of
    [] -> NoInit
    exprs -> ExprInit (ListExpr noPos exprs)
  unVarForInit init            = init
  
  unVarForInInit :: ForInInit SourcePos -> ForInInit SourcePos
  unVarForInInit (ForInVar id) = ForInNoVar id
  unVarForInInit init          = init

 
removeInnerVarDecls expr = expr


removeForStmt :: Statement SourcePos -> Statement SourcePos
removeForStmt (ForStmt p init maybeTest maybeIncr body) = stmts where
  stmts = BlockStmt p [prelude,WhileStmt p whileTest whileBody]
  prelude = case init of
    NoInit -> EmptyStmt p
    ExprInit e -> ExprStmt p e
    -- following shouldn't happen, but allows this function to be used alone
    VarInit decls -> VarDeclStmt p decls
  whileTest = case maybeTest of
    Nothing -> BoolLit p True
    Just e -> e
  whileBody = BlockStmt p [body,whileIncr]
  whileIncr = case maybeIncr of
    Nothing -> EmptyStmt p
    Just e -> ExprStmt p e
removeForStmt stmt = stmt 

-- TODO: This won't work.  A break/continue in the body will fail.
removeDoWhileStmt :: Statement SourcePos -> Statement SourcePos
removeDoWhileStmt (DoWhileStmt p body test) = stmts where
  stmts = BlockStmt p [body,WhileStmt p test body]
removeDoWhileStmt stmt = stmt

removeIfSingleStmt :: Statement SourcePos -> Statement SourcePos
removeIfSingleStmt (IfSingleStmt p test body) =
  IfStmt p test body (EmptyStmt p)
removeIfSingleStmt stmt = stmt

simplifyStmts = removeForStmt.removeDoWhileStmt.removeIfSingleStmt

-- |Applied bottom up, so it can assume its children are simplified
simplifyBlocks :: Statement SourcePos -> Statement SourcePos
simplifyBlocks (BlockStmt p stmts) = result where
  result = case concatMap simpl stmts of
    [] -> EmptyStmt p
    [stmt] -> stmt
    stmts' -> BlockStmt p stmts'
  simpl (EmptyStmt{}) = []
  simpl (BlockStmt p stmts) = stmts
  simpl stmt = [stmt]
simplifyBlocks stmt = stmt 

removePseudoEmptyStmts :: Statement SourcePos -> Statement SourcePos
removePseudoEmptyStmts (VarDeclStmt p []) = EmptyStmt p
removePseudoEmptyStmts stmt = stmt

simplify :: [Statement SourcePos] -> [Statement SourcePos]
simplify script =  topBinds:topBlocksRemoved where
  topBlocksRemoved = concatMap removeBlock resimplified
  removeBlock (BlockStmt _ stmts) = stmts
  removeBlock stmt = [stmt]
  resimplified = everywhere (mkT simplifyBlocks)
    $ everywhereBut (not.excludeFunctions) (mkT removeVarDecl)
    $ everywhere (mkT removePseudoEmptyStmts)
    $ everywhere (mkT removeInnerVarDecls)
    $ everywhere (mkT pseudoLetBindings) purified
  purified = evalState (mapM (purifyStmt []) simplified) 0 -- TODO: prolly wrong
  simplified = everywhere (mkT simplifyStmts)
    $ everywhere (mkT removeInnerVarDecls)
    $ everywhere (mkT pseudoLetBindings)
    $ everywhere (mkT removeFuncStmt) script
 
  topVars = S.toList (localVars purified) -- TODO: wrong. Use globalVars!
  topBinds = VarDeclStmt noPos $
    map (\name -> VarDecl noPos (Id noPos name) Nothing) topVars

assign :: Expression SourcePos -> Expression SourcePos -> Statement SourcePos
assign e1 e2 = ExprStmt noPos (AssignExpr noPos OpAssign e1 e2)

newLocalVar :: Expression SourcePos 
            -> State Int (Statement SourcePos,Expression SourcePos)
newLocalVar expr = do
  n <- get
  put (n+1)
  let id = Id noPos ("__webbits" ++ show n)
  return (VarDeclStmt noPos [VarDecl noPos id (Just expr)],VarRef noPos id)

newStmtLabel :: State Int (Id SourcePos)
newStmtLabel = do
  n <- get
  put (n+1)
  return (Id noPos ("__webbitslabel" ++ show n))
   

-- |If the expression is not a variable-reference, create a name for it.
needVar :: Expression SourcePos
        -> State Int (Statement SourcePos,Expression SourcePos)
needVar expr = case expr of
  VarRef p _ -> return (EmptyStmt p,expr)
  otherwise -> newLocalVar expr

-- These checks is necessary because the following transformation is illegal:
--
-- f.x()
--
-- to
--
-- t0 = f.x;
-- t1 = t0()
--
-- and similarly for f["x"]()
purifyCall p (DotRef p' obj method) args = do
  (objStmts,objExpr) <- purifyExpr obj
  r <- mapM purifyExpr args
  let (argsStmts,argExprs) = (concatMap fst r,map snd r)
  (s1,objExpr') <- needVar objExpr
  r <- mapM needVar argExprs
  let (ss2,argExprs') = unzip r
  (decl,ref) <- newLocalVar $ CallExpr p (DotRef p' objExpr' method) argExprs'
  return (objStmts ++ argsStmts ++ (s1:ss2) ++ [decl],ref)
purifyCall p (BracketRef p' obj method) args = do
  (objStmts,objExpr) <- purifyExpr obj
  (methodStmts,methodExpr) <- purifyExpr method
  r <- mapM purifyExpr args
  let (argsStmts,argExprs) = (concatMap fst r,map snd r)
  (s1,objExpr') <- needVar objExpr
  r <- mapM needVar argExprs
  let (ss2,argExprs') = unzip r
  (s3,methodExpr') <- needVar methodExpr
  (decl,ref) <- newLocalVar $ 
    CallExpr p (BracketRef p' objExpr' methodExpr') argExprs'
  return (objStmts ++ methodStmts ++ argsStmts ++ (s1:s3:ss2) ++ [decl],ref)
purifyCall p fn args = do
  (fnStmts,fnExpr) <- purifyExpr fn
  r <- mapM purifyExpr args
  let (argsStmts,argExprs) = (concatMap fst r,map snd r)
  (s1,fnExpr') <- needVar fnExpr
  r <- mapM needVar argExprs
  let (ss2,argExprs') = unzip r
  (decl,ref) <- newLocalVar $ CallExpr p fnExpr' argExprs'
  return (fnStmts ++ argsStmts ++ (s1:ss2) ++ [decl],ref)

-- |Lifts functions calls out of expressions into a sequence of
-- statements.
purifyExpr :: Expression SourcePos 
           -> State Int ([Statement SourcePos],Expression SourcePos)
purifyExpr expr = case expr of
  StringLit{} -> return ([],expr)
  RegexpLit{} -> return ([],expr)
  NumLit{} -> return ([],expr)
  BoolLit{} -> return ([],expr)
  NullLit{} -> return ([],expr)
  ArrayLit p es -> do
    r <- mapM purifyExpr es
    return (concatMap fst r,ArrayLit p $ map snd r)
  VarRef{} -> return ([],expr)
  CondExpr p e1 e2 e3 -> do 
    (e1Stmts,e1Expr) <- purifyExpr e1
    (e2Stmts,e2Expr) <- purifyExpr e2
    (e3Stmts,e3Expr) <- purifyExpr e3
    (decl,ref) <- newLocalVar e1Expr
    let e2Stmts' = BlockStmt p [BlockStmt p e2Stmts,assign ref e2Expr]
    let e3Stmts' = BlockStmt p [BlockStmt p e3Stmts,assign ref e3Expr]
    return ([BlockStmt p e1Stmts,IfStmt p ref e2Stmts' e3Stmts'],ref)
  ParenExpr p e -> purifyExpr e
  CallExpr p fn args -> purifyCall p fn args
  NewExpr p constr args -> do
    (constrStmts,constr') <- purifyExpr constr
    r <- mapM purifyExpr args
    let argsStmts = concatMap fst r
    let args' = map snd r
    (s1,constr'') <- needVar constr'
    r <- mapM needVar args'
    let (ss2,args'') = unzip r
    (decl,ref) <- newLocalVar $ NewExpr p constr'' args''
    return (constrStmts ++ argsStmts ++ (s1:ss2) ++ [decl],ref)
  FuncExpr p args body -> do
    expr <- liftM (FuncExpr p args) (purifyStmt [] body)
    return ([],expr)
  AssignExpr p op lhs rhs -> do
    (rhsStmts,rhs') <- purifyExpr rhs
    (lhsStmts,lhs') <- purifyExpr lhs
    let stmts = lhsStmts ++ rhsStmts ++ 
                [ExprStmt p (AssignExpr p op lhs' rhs')]
    return (stmts,lhs')
  ListExpr p es -> do
    r <- mapM purifyExpr es
    return (concatMap fst r,snd $ L.last r) -- discard earlier expressions
  InfixExpr p op lhs rhs
    | op == OpLAnd -> do
      (lhsStmts,lhs') <- purifyExpr lhs
      (rhsStmts,rhs') <- purifyExpr rhs
      case rhsStmts of
        [] -> return (lhsStmts,InfixExpr p op lhs' rhs')
        otherwise -> do
          (decl,ref) <- newLocalVar lhs'
          let rhsStmts' = rhsStmts ++ [assign ref rhs']
          return ([BlockStmt p lhsStmts,
                   IfStmt p ref (BlockStmt p rhsStmts') (EmptyStmt p)],
                  ref)
    | otherwise -> do
      (lhsStmts,lhs') <- purifyExpr lhs
      (rhsStmts,rhs') <- purifyExpr rhs
      return (lhsStmts ++ rhsStmts, InfixExpr p op lhs' rhs')
  PostfixExpr p op e -> do
    (eStmts,e') <- purifyExpr e
    (declStmt,id) <- newLocalVar e'
    let modifyExpr = AssignExpr p OpAssign e' $ case op of
          PostfixInc -> InfixExpr p OpAdd e' (NumLit p 1)
          PostfixDec -> InfixExpr p OpSub e' (NumLit p 1) 
    return (eStmts ++ [declStmt,ExprStmt p modifyExpr],id)
  PrefixExpr p op e -> do
    (eStmts,e') <- purifyExpr e
    case op of
      PrefixInc ->
        return (eStmts ++ [assign e' (InfixExpr p OpAdd e' (NumLit p 1))],e')
      PrefixDec ->
        return (eStmts ++ [assign e' (InfixExpr p OpSub e' (NumLit p 1))],e')
      PrefixDelete -> do
        (decl0,e'') <- needVar e'
        (decl1,ref) <- newLocalVar (PrefixExpr p PrefixDelete e'')
        return (eStmts ++ [decl0,decl1],ref)
      otherwise -> 
        return (eStmts,PrefixExpr p op e')
  ObjectLit p pes -> do
    let (props,exprs) = unzip pes
    r <- mapM purifyExpr exprs
    return (concatMap fst r, ObjectLit p $ zip props (map snd r))
  ThisRef p -> return ([],expr)
  DotRef p e id -> do
    (eStmts,e') <- purifyExpr e
    return (eStmts,DotRef p e' id)
  BracketRef p e1 e2 -> do
    (e1Stmts,e1') <- purifyExpr e1
    (e2Stmts,e2') <- purifyExpr e2
    return (e1Stmts ++ e2Stmts,BracketRef p e1' e2')


data LabelType 
  = ExplicitLoopLabel
  | ExplicitLabel
  | ImplicitLoopLabel
  | ImplicitSwitchLabel

isExplicitLabel (_,ExplicitLoopLabel) = True
isExplicitLabel (_,ExplicitLabel) = True
isExplicitLabel _ = False

isLoopLabel (_,ExplicitLoopLabel) = True
isLoopLabel (_,ImplicitLoopLabel) = True
isLoopLabel _ = False

isLoop (ForStmt{}) = True
isLoop (ForInStmt{}) = True
isLoop (WhileStmt{}) = True
isLoop (DoWhileStmt{}) = True
isLoop (LabelledStmt _ _ s) = isLoop s
isLoop _ = False

-- |The first argument is a list of enclosing labels, initially empty for each
-- function.  We cons labels from labelled statements onto this list.  In
-- addition, we generate labels for switch statements and loops.  This allows
-- us to make all continue and break statements jump/exit to explicit 
-- statements.  
purifyStmt :: [(Id SourcePos,LabelType)]
           -> Statement SourcePos -> State Int (Statement SourcePos)
purifyStmt labels stmt = case stmt of
  BlockStmt p stmts -> liftM (BlockStmt p) (mapM (purifyStmt labels) stmts)
  EmptyStmt{} -> return stmt
  ExprStmt p e -> do
    (stmts,_) <- purifyExpr e -- discard the pure expression
    return $ if null stmts then (EmptyStmt p) else (BlockStmt p stmts)
  IfStmt p e s1 s2 -> do
    (ess,e') <- purifyExpr e
    s1' <- purifyStmt labels s1
    s2' <- purifyStmt labels s2
    return (BlockStmt p [BlockStmt p ess,IfStmt p e' s1' s2'])
  SwitchStmt p e cases -> do
    (eStmts,e') <- purifyExpr e
    (s1,id) <- needVar e'
    l <- newStmtLabel
    cases' <- mapM (purifyCase ((l,ImplicitSwitchLabel):labels)) cases
    return $ BlockStmt p [BlockStmt p eStmts,
                          s1,
                          LabelledStmt p l $ SwitchStmt p id cases']
  WhileStmt p e s -> do
    (ess,e') <- purifyExpr e
    l <- newStmtLabel
    s' <- purifyStmt ((l,ImplicitLoopLabel):labels) s
    return (BlockStmt p [BlockStmt p ess,
                         LabelledStmt p l $ WhileStmt p e' s'])
  -- an unlabelled break terminates the innermost enclosing switch/loop.
  BreakStmt p Nothing -> case L.find (not.isExplicitLabel) labels of
    Just (id,lbl) -> return $ BreakStmt p (Just id)
    Nothing -> fail $ "syntax error at " ++ show p ++ "\nNo enclosing loop " ++
                      "or switch to break to."
  BreakStmt p (Just id) -> case L.lookup id labels of
    Just _ -> return stmt
    Nothing -> fail $ "syntax error at " ++ show p ++ "\nNo enclosing label " ++
                      "with the name " ++ show id
  -- continues the next iteration of the innermost enclosing loop.
  ContinueStmt p Nothing -> case L.find isLoopLabel labels of
    Nothing -> fail $ "syntax error at " ++ show p ++ "\nUse of continue " ++
                     "outside a loop"
    Just (id,_) -> return $ ContinueStmt p (Just id)
  ContinueStmt p (Just id) -> case L.lookup id labels of
    Nothing -> fail $ "syntax error at " ++ show p ++ "\nNo loop named " ++
                      show id
    -- we can't have an explicit continue to an implicitly labelled loop
    Just ExplicitLoopLabel -> return stmt
    otherwise -> fail $ "syntax error at " ++ show p ++ "\ncontinue jumping " ++
                        "to a statement that is not a loop."
    
  ForInStmt p init expr body -> do
    (exprStmts,expr') <- purifyExpr expr
    l <- newStmtLabel
    body' <- purifyStmt ((l,ImplicitLoopLabel):labels) body
    return (BlockStmt p [BlockStmt p exprStmts,ForInStmt p init expr' body']) 
  TryStmt p body catches Nothing -> do
    body' <- purifyStmt labels body
    catches' <- mapM (purifyCatch labels) catches
    return (TryStmt p body' catches' Nothing)
  TryStmt p body catches (Just finally) -> do
    body' <- purifyStmt labels body
    catches' <- mapM (purifyCatch labels) catches
    finally' <- purifyStmt labels finally
    return (TryStmt p body' catches' (Just finally'))
  ThrowStmt p e -> do
    (es,e') <- purifyExpr e
    return (BlockStmt p [BlockStmt p es,ThrowStmt p e'])
  ReturnStmt p Nothing -> return stmt
  ReturnStmt p (Just e) -> do
    (es,e') <- purifyExpr e
    return (BlockStmt p [BlockStmt p es,ReturnStmt p (Just e')])
  WithStmt{} -> fail "cannot handle With yet"
  VarDeclStmt p decls -> do
    r <- mapM purifyDecl decls
    return (BlockStmt p [BlockStmt p (concatMap fst r), 
                         VarDeclStmt p (map snd r)])
  LabelledStmt p id s -> case isLoop s of
    True -> do s' <- purifyStmt ((id,ExplicitLoopLabel):labels) s
               return $ LabelledStmt p id s'
    False -> do s' <- purifyStmt ((id,ExplicitLabel):labels) s
                return $ LabelledStmt p id s'
  otherwise -> fail $ "purifyExpr received " ++ show stmt

purifyDecl :: VarDecl SourcePos 
           -> State Int ([Statement SourcePos],VarDecl SourcePos)
purifyDecl decl@(VarDecl p id rhs) = case rhs of
  Nothing -> return ([],decl)
  Just expr -> do
    (ss,e) <- purifyExpr expr
    return (ss,VarDecl p id (Just e))

-- TODO: binding for id?
purifyCatch labels (CatchClause p id s) = do
  s' <- purifyStmt labels s
  return (CatchClause p id s')


-- |Assumes that the head of 'labels' is an 'ImplicitSwitchLabel'.  After
-- purification, the list of statements in the body is a single block
-- statement.
purifyCase :: [(Id SourcePos,LabelType)]
           -> CaseClause SourcePos -> State Int (CaseClause SourcePos)
purifyCase labels (CaseDefault p ss) = do
  s' <- purifyStmt labels (BlockStmt p ss)
  return (CaseDefault p [s'])
purifyCase labels (CaseClause p e ss) = do
  (es,e') <- purifyExpr e
  case es of
    [] -> do s' <- purifyStmt labels (BlockStmt p ss)
             return (CaseClause p e' [s'])
    otherwise -> fail $ "semantic error(!!!) at " ++ show p ++ "\n" ++
                        "the case expression should be side-effect free"
