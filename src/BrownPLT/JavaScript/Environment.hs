module BrownPLT.JavaScript.Environment
  ( env
  , localVars
  , EnvTree (..)
  ) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Text.ParserCombinators.Parsec.Pos (SourcePos)

import BrownPLT.JavaScript.Syntax

-- Intermediate data structure that contains locally declared names and
-- all references to identifers.
data Partial = Partial {
  partialLocals :: M.Map String SourcePos,
  partialReferences :: M.Map String SourcePos,
  partialNested :: [Partial]
}

empty :: Partial
empty = Partial M.empty M.empty []

ref :: Id SourcePos -> Partial
ref (Id p v) = Partial M.empty (M.singleton v p) []

decl :: Id SourcePos -> Partial
decl (Id p v) = Partial (M.singleton v p) M.empty []

nest :: Partial -> Partial
nest partial = Partial M.empty M.empty [partial]

-- Combine partial results from the same lexical scope.
unions :: [Partial] -> Partial
unions ps = Partial (M.unions (map partialLocals ps))
                    (M.unions (map partialReferences ps))
                    (concatMap partialNested ps)

javascript :: JavaScript SourcePos -> Partial
javascript (Script _ ss) = unions (map stmt ss)


lvalue :: LValue SourcePos -> Partial
lvalue lv = case lv of
  LVar p x -> ref (Id p x)
  LDot _ e _ -> expr e
  LBracket _ e1 e2 -> unions [expr e1, expr e2]

expr :: Expression SourcePos -> Partial
expr e = case e of
  StringLit _ _ -> empty
  RegexpLit _ _ _ _ -> empty
  NumLit _ _ -> empty
  IntLit _ _ -> empty
  BoolLit _ _ -> empty
  NullLit _ -> empty
  ArrayLit _ es -> unions (map expr es)
  ObjectLit _ props -> unions (map (expr.snd) props)
  ThisRef _ -> empty
  VarRef _ id -> empty
  DotRef _ e _ -> expr e
  BracketRef _ e1 e2 -> unions [expr e1, expr e2]
  NewExpr _ e1 es -> unions [expr e1, unions $ map expr es]
  PrefixExpr _ _ e -> expr e
  InfixExpr _ _ e1 e2 -> unions [expr e1, expr e2]
  CondExpr _ e1 e2 e3 -> unions [expr e1, expr e2, expr e3]
  AssignExpr _ _ lv e -> unions [lvalue lv, expr e]
  UnaryAssignExpr _ _ lv -> lvalue lv
  ParenExpr _ e -> expr e
  ListExpr _ es -> unions (map expr es)
  CallExpr _ e es -> unions [expr e, unions $ map expr es]
  FuncExpr _ args s -> nest $ unions [unions $ map decl args, stmt s]

caseClause :: CaseClause SourcePos -> Partial
caseClause cc = case cc of
  CaseClause _ e ss -> unions [expr e, unions $ map stmt ss]
  CaseDefault _ ss -> unions $ map stmt ss

-- TODO: Verify that this is a declaration and not a reference.
catchClause :: CatchClause SourcePos -> Partial
catchClause (CatchClause _ id s) = unions [decl id, stmt s]

varDecl :: VarDecl SourcePos -> Partial
varDecl (VarDecl _ id Nothing) = decl id
varDecl (VarDecl _ id (Just e)) = unions [decl id, expr e]
 
forInit :: ForInit SourcePos -> Partial
forInit fi = case fi of
  NoInit -> empty
  VarInit ds -> unions $ map varDecl ds
  ExprInit e -> expr e 

forInInit :: ForInInit SourcePos -> Partial
forInInit (ForInVar id) = decl id
forInInit (ForInNoVar id) = ref id
  
stmt :: Statement SourcePos -> Partial
stmt s = case s of
  BlockStmt _ ss -> unions $ map stmt ss
  EmptyStmt _ -> empty
  ExprStmt _ e -> expr e
  IfStmt _ e s1 s2 -> unions [expr e, stmt s1, stmt s2]
  IfSingleStmt _ e s -> unions [expr e, stmt s]
  SwitchStmt _ e cases -> unions [expr e, unions $ map caseClause cases]
  WhileStmt _ e s -> unions [expr e, stmt s]
  DoWhileStmt _ s e -> unions [stmt s, expr e]
  BreakStmt _ _ -> empty
  ContinueStmt _ _ -> empty
  LabelledStmt _ _ s -> stmt s
  ForInStmt _ fii e s -> unions [forInInit fii, expr e, stmt s]
  ForStmt _ fi  me1 me2 s -> 
    unions [forInit fi, maybe empty expr me1, maybe empty expr me2, stmt s]
  TryStmt _ s catches ms ->
    unions [stmt s, unions $ map catchClause catches, maybe empty stmt ms]
  ThrowStmt _ e -> expr e
  ReturnStmt _ me -> maybe empty expr me
  WithStmt _ e s -> unions [expr e, stmt s]
  VarDeclStmt _ decls -> unions $ map varDecl decls
  FunctionStmt _ fnId args s ->
    unions [decl fnId, nest $ unions [unions $ map decl args, stmt s]]

-- |The statically-determinate lexical structure of a JavaScript program.
data EnvTree = EnvTree (M.Map String SourcePos) [EnvTree]

-- A 'Partial' specifies identifier references in addition to identifier
-- declarations.  We descend into a 'Partial', pushing enclosing declarations
-- in to remove references to identifiers declared in the enclosing scope.
-- Any referencs to identifiers not declared in either the current or the
-- enclosing scope are local definitions of global variables.
makeEnvTree :: Map String SourcePos -- ^enclosing environment
            -> Partial -- ^local environment and references
            -> (EnvTree,Map String SourcePos) 
            -- ^environment and global definitions
makeEnvTree enclosing (Partial locals references nested) = (tree,globals) where
  nestedResults = map (makeEnvTree (locals `M.union` enclosing)) nested
  tree = EnvTree locals (map fst nestedResults)
  globals' = (references `M.difference` locals) `M.difference` enclosing
  globals = M.unions (globals':(map snd nestedResults))

env :: Map String SourcePos -- ^browser/testing environment
    -> [Statement SourcePos] 
    -> (EnvTree,Map String SourcePos)
env globals program = makeEnvTree globals (unions $ map stmt program)


localVars :: [Statement SourcePos]
          -> [(String, SourcePos)]
localVars body = M.toList locals where
  Partial locals _ _ = unions $ map stmt body

