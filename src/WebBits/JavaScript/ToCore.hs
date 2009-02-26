-- |Transforms simplified JavaScript syntax to Core syntax.
module WebBits.JavaScript.ToCore
  ( jsToCore
  ) where

import Control.Monad
import Data.Generics
import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Foldable as Foldable

import WebBits.Common (pp)
import Text.PrettyPrint.HughesPJ (render)
import qualified WebBits.JavaScript.Core as Core
import WebBits.JavaScript
import WebBits.JavaScript.Simplify (simplify)


numberStmts :: [Statement SourcePos] -> [Statement (Int,SourcePos)]
numberStmts stmts = evalState (gmapM (mkM numberM) stmts') 0 where
  stmts' :: [Statement (Int,SourcePos)]
  stmts' = map (fmap (\p -> (0,p))) stmts
  numberM :: (Int,SourcePos) -> State Int (Int,SourcePos)
  numberM (_,p) = do
    n <- get
    put (n+1)
    return (n,p)

jsToCore :: [Statement SourcePos]
         -> [Core.Stmt (Int,SourcePos)]
jsToCore (VarDeclStmt{}:stmts) = map (stmt M.empty) (numberStmts stmts)
jsToCore stmts = error $ "jsToCore: missing global vars:\n" ++ show stmts

unId (Id _ v) = v

unVar (VarRef _ (Id _ v)) = v
unVar e = error $ "expected a VarRef:\n" ++ show e

unDecl (VarDecl _ (Id _ v) Nothing) = v
unDecl d = error "expected a variable declaration without an RHS:\n" ++ (show d)

unInfix op = case op of
  OpLT -> Core.OpLT
  OpLEq -> Core.OpLEq
  OpGT -> Core.OpGT
  OpGEq  -> Core.OpGEq
  OpIn  -> Core.OpIn
  OpInstanceof -> Core.OpInstanceof
  OpEq -> Core.OpEq
  OpNEq -> Core.OpNEq
  OpStrictEq -> Core.OpStrictEq
  OpStrictNEq -> Core.OpStrictNEq
  OpLAnd -> Core.OpLAnd
  OpLOr -> Core.OpLOr
  OpMul -> Core.OpMul
  OpDiv -> Core.OpDiv
  OpMod  -> Core.OpMod
  OpSub -> Core.OpSub
  OpLShift -> Core.OpLShift
  OpSpRShift -> Core.OpSpRShift
  OpZfRShift -> Core.OpZfRShift
  OpBAnd -> Core.OpBAnd
  OpBXor -> Core.OpBXor
  OpBOr -> Core.OpBOr
  OpAdd -> Core.OpAdd

unPrefix op = case op of 
  PrefixLNot -> Core.PrefixLNot
  PrefixBNot -> Core.PrefixBNot
  PrefixMinus -> Core.PrefixMinus
  PrefixTypeof -> Core.PrefixTypeof
  PrefixVoid -> Core.PrefixVoid
  otherwise -> error $ "unPrefix cannot translate:\n" ++ show op

unlabelled (LabelledStmt _ _ s) = unlabelled s
unlabelled s = s

getLabel f = case Foldable.find (const True) f of
  Just a -> a
  Nothing -> error "getLabel failed"

-- Takes a map from string labels to statement numbers
stmt :: M.Map String Int
     -> Statement (Int,SourcePos)
     -> Core.Stmt (Int,SourcePos)
stmt lbl (BlockStmt p ss) = Core.SeqStmt p (map (stmt lbl) ss)
stmt lbl (EmptyStmt p) = Core.EmptyStmt p
stmt lbl (ExprStmt p (AssignExpr _ OpAssign (VarRef _ (Id _ r))  rhs)) = 
  case rhs of
    CallExpr p (DotRef _ f id) args ->
      Core.MethodCallStmt p r (unVar f) (unId id) (map unVar args)
    CallExpr p (BracketRef _ f m) args ->
      Core.IndirectMethodCallStmt p r (unVar f) (unVar m) (map unVar args)
    CallExpr p f args -> Core.CallStmt p r (unVar f) (map unVar args)
    NewExpr p f args -> Core.NewStmt p r (unVar f) (map unVar args)
    PrefixExpr p PrefixDelete id -> Core.DeleteStmt p r (unVar id)
    e -> Core.AssignStmt p r (expr e) 
stmt lbl (IfStmt p e s1 s2) = Core.IfStmt p (expr e) (stmt lbl s1) (stmt lbl s2)
stmt lbl (WhileStmt p e s) = Core.WhileStmt p (expr e) (stmt lbl s)
stmt lbl (ForInStmt p (ForInNoVar id) e s) =
  Core.ForInStmt p (unId id) (expr e) (stmt lbl s)
stmt lbl (ReturnStmt p maybeE) = Core.ReturnStmt p (liftM expr maybeE)
stmt lbl (LabelledStmt p (Id _ id) s) =
  stmt (M.insert id (fst $ getLabel $ unlabelled s) lbl) s
stmt lbl (BreakStmt p (Just (Id _ id))) = case M.lookup id lbl of
  Just n -> Core.BreakStmt p n
  Nothing -> error "invalid label for break"
stmt lbl (ContinueStmt p (Just (Id _ id))) = case M.lookup id lbl of
  Just n -> Core.ContinueStmt p n
  Nothing -> error "invalid label for break"

stmt _ s = error $ "cannot translate this statement to core syntax:\n" ++ 
  (render $ pp s) ++ "\n" ++ show s

field (PropString _ s,e) = (Left s,expr e)
field (PropNum _ n,e) = (Right n,expr e)
field (PropId _ (Id _ s),e) = (Left s,expr e) 

expr (StringLit p s) =  Core.Lit (Core.StringLit p s)
expr (RegexpLit p s b0 b1) = Core.Lit (Core.RegexpLit p s b0 b1)
expr (NumLit p x) = Core.Lit (Core.NumLit p x)
expr (BoolLit p b) = Core.Lit (Core.BoolLit p b)
expr (NullLit p) = Core.Lit (Core.NullLit p)
expr (ArrayLit p es) = Core.Lit (Core.ArrayLit p (map expr es))
expr (ObjectLit p fields) = Core.Lit (Core.ObjectLit p $ map field fields)
expr (VarRef p (Id _ v)) = Core.VarRef p v
expr (FuncExpr p args (BlockStmt _ ((VarDeclStmt p' decls):body))) = 
 Core.FuncExpr p (map unId args) (map unDecl decls) 
               (stmt M.empty (BlockStmt p' body)) 
expr (InfixExpr p op lhs rhs) =
 Core.OpExpr p (unInfix op) [expr lhs,expr rhs] 
expr (PrefixExpr p op e) = Core.OpExpr p (unPrefix op) [expr e]
expr (ThisRef p) = Core.This p
expr (DotRef p e id) = Core.DotRef p (expr e) (unId id) 
expr (BracketRef p e1 e2) = Core.BracketRef p (expr e1) (expr e2)
expr e = error $ "cannot translate this expression to core syntax:\n" ++ 
  (render $ pp e)
