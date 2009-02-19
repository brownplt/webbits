module WebBits.JavaScript.CoreTransform 
  ( simplify
  ) where

import qualified Data.Set as S
import qualified Data.Maybe as Y
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

 
  removeVarDecl :: Statement SourcePos -> Statement SourcePos
  removeVarDecl (VarDeclStmt p decls) = case Y.mapMaybe unDecl decls of
    [] -> EmptyStmt p
    exprs -> ExprStmt p (ListExpr p exprs)
  removeVarDecl stmt                  = stmt

  unDecl :: VarDecl SourcePos -> Maybe (Expression SourcePos)
  unDecl (VarDecl p id Nothing) = Nothing
  unDecl (VarDecl p id (Just e)) = Just $ AssignExpr p OpAssign (VarRef p id) e 
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

removeDoWhileStmt :: Statement SourcePos -> Statement SourcePos
removeDoWhileStmt (DoWhileStmt p body test) = stmts where
  stmts = BlockStmt p [body,WhileStmt p test body]
removeDoWhileStmt stmt = stmt

removeIfSingleStmt :: Statement SourcePos -> Statement SourcePos
removeIfSingleStmt (IfSingleStmt p test body) =
  IfStmt p test body (EmptyStmt p)
removeIfSingleStmt stmt = stmt

simplifyStmts = removeForStmt.removeDoWhileStmt.removeIfSingleStmt
 

simplify :: [Statement SourcePos] -> [Statement SourcePos]
simplify script =  everywhere (mkT simplifyStmts)
  $ everywhere (mkT removeInnerVarDecls)
  $ everywhere (mkT pseudoLetBindings)
  $ everywhere (mkT removeFuncStmt) script


-- |Lifts functions calls out of expressions into a sequence of
-- statements.
sepEffects :: Expression SourcePos 
           -> m ([Statement SourcePos],Expression SourcePos)
sepEffects expr = case expr of
  StringLit{} -> return expr
  RegexpLit{} -> return expr
  NumLit{} -> return expr
  BoolLit -> return expr
  NullLit{} -> return expr
  ArrayLit p es -> do
    r <- mapM sepEffects es
    return (concatMap fst r,map snd r)
  CallExpr p fn args -> do
    (fnStmts,fnExpr) <- sepEffects fn
    r <- mapM sepEffects fn
    let (argsStmts,argExprs) = (concatMap fst r,map snd r)
    return (fnStmts ++ argsStmts,CallExpr p fnExpr argExprs)

{-
data Expression a
  = StringLit a String
  | RegexpLit a String Bool {- global? -} Bool {- case-insensitive? -}
  | NumLit a Double -- pg. 5 of ECMA-262
  | BoolLit a Bool
  | NullLit a
  | ArrayLit a [Expression a]
  | ObjectLit a [(Prop a, Expression a)]
  | ThisRef a
  | VarRef a (Id a)
  | DotRef a (Expression a) (Id a)
  | BracketRef a (Expression a) {- container -} (Expression a) {- key -}
  | NewExpr a (Expression a) {- constructor -} [Expression a]
  | PostfixExpr a PostfixOp (Expression a)
  | PrefixExpr a PrefixOp (Expression a)
  | InfixExpr a InfixOp (Expression a) (Expression a)
  | CondExpr a (Expression a) (Expression a) (Expression a)
  | AssignExpr a AssignOp (Expression a) (Expression a)
  | ParenExpr a (Expression a)
  | ListExpr a [Expression a]
  | CallExpr a (Expression a) [Expression a]
  | FuncExpr a [(Id a)] (Statement a)
  deriving (Show,Data,Typeable,Eq,Ord)

  
data Statement a
  = BlockStmt a [Statement a]
  | EmptyStmt a
  | ExprStmt a (Expression a)
  | IfStmt a (Expression a) (Statement a) (Statement a)
  | SwitchStmt a (Expression a) [CaseClause a]
  | WhileStmt a (Expression a) (Statement a)
  | BreakStmt a (Maybe (Id a))
  | ContinueStmt a (Maybe (Id a))
  | LabelledStmt a (Id a) (Statement a)
  | ForInStmt a (ForInInit a) (Expression a) (Statement a)
  | TryStmt a (Statement a) {-body-} [CatchClause a] {-catches-}
      (Maybe (Statement a)) {-finally-}
  | ThrowStmt a (Expression a)
  | ReturnStmt a (Maybe (Expression a))
  | WithStmt a (Expression a) (Statement a)
  deriving (Show,Data,Typeable,Eq,Ord)  

-}
