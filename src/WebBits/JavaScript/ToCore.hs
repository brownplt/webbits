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

jsToCore :: [Statement SourcePos]
         -> [Core.Stmt SourcePos]
jsToCore (VarDeclStmt{}:stmts) = map stmt stmts
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
stmt :: Statement SourcePos
     -> Core.Stmt SourcePos
stmt (BlockStmt p ss) = Core.SeqStmt p (map (stmt) ss)
stmt (EmptyStmt p) = Core.EmptyStmt p
stmt (ExprStmt p (AssignExpr _ OpAssign (VarRef _ (Id _ r))  rhs)) = 
  case rhs of
    CallExpr p (DotRef _ f id) args ->
      Core.MethodCallStmt p r (unVar f) (unId id) (map unVar args)
    CallExpr p (BracketRef _ f m) args ->
      Core.IndirectMethodCallStmt p r (unVar f) (unVar m) (map unVar args)
    CallExpr p f args -> Core.CallStmt p r (unVar f) (map unVar args)
    NewExpr p f args -> Core.NewStmt p r (unVar f) (map unVar args)
    PrefixExpr p PrefixDelete id -> Core.DeleteStmt p r (unVar id)
    e -> Core.AssignStmt p r (expr e) 
stmt (IfStmt p e s1 s2) = Core.IfStmt p (expr e) (stmt s1) (stmt s2)
stmt (WhileStmt p e s) = Core.WhileStmt p (expr e) (stmt s)
stmt (ForInStmt p (ForInNoVar id) e s) =
  Core.ForInStmt p (unId id) (expr e) (stmt s)
stmt (ReturnStmt p maybeE) = Core.ReturnStmt p (liftM expr maybeE)
stmt (LabelledStmt p id s) = Core.LabelledStmt p (unId id) (stmt s)
stmt (BreakStmt p (Just id)) = Core.BreakStmt p (unId id)
stmt (ContinueStmt p (Just id)) = Core.ContinueStmt p (unId id)
stmt (SwitchStmt p (VarRef _ v) cases) = 
  Core.SwitchStmt p (unId v) (map case_ cases) where
    case_ (CaseClause _ e [s]) = (lit e,stmt s)
    case_ c = error $ "case_: cannot translate to ANF syntax:\n" ++ show c
stmt s = error $ "cannot translate this statement to core syntax:\n" ++ 
  (render $ pp s) ++ "\n" ++ show s

field (PropString _ s,e) = (Left s,expr e)
field (PropNum _ n,e) = (Right n,expr e)
field (PropId _ (Id _ s),e) = (Left s,expr e) 

lit e = unLit (expr e) where
  unLit (Core.Lit l) = l
  unLit e = error $ "unLit: not a literal " ++ show e

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
               (stmt (BlockStmt p' body)) 
expr (InfixExpr p op lhs rhs) =
 Core.OpExpr p (unInfix op) [expr lhs,expr rhs] 
expr (PrefixExpr p op e) = Core.OpExpr p (unPrefix op) [expr e]
expr (ThisRef p) = Core.This p
expr (DotRef p e id) = Core.DotRef p (expr e) (unId id) 
expr (BracketRef p e1 e2) = Core.BracketRef p (expr e1) (expr e2)
expr e = error $ "cannot translate this expression to core syntax:\n" ++ 
  (show e)
