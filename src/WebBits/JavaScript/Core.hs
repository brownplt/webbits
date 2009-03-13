module WebBits.JavaScript.Core 
  ( Id
  , FOp(..)
  , Lit(..)
  , Expr(..)
  , Stmt(..)
  , stmtLabel
  ) where

import Data.Generics
import Control.Arrow (first,second,(***))
import Text.Printf

type Id = String

-- |Pure functional
data FOp = OpLT | OpLEq | OpGT | OpGEq  | OpIn  | OpInstanceof | OpEq | OpNEq
  | OpStrictEq | OpStrictNEq | OpLAnd | OpLOr | OpMul | OpDiv | OpMod  | OpSub 
  | OpLShift | OpSpRShift | OpZfRShift | OpBAnd | OpBXor | OpBOr | OpAdd
  | PrefixLNot | PrefixBNot | PrefixMinus | PrefixTypeof | PrefixVoid
  deriving (Show,Data,Typeable,Eq,Ord)


data Lit a
  = StringLit a String
  | RegexpLit a String Bool {- global? -} Bool {- case-insensitive? -}
  | NumLit a Double
  | IntLit a Integer
  | BoolLit a Bool
  | NullLit a
  | ArrayLit a [Expr a]
  | ObjectLit a [(Either String Integer, Expr a)]
  deriving (Show,Data,Typeable,Eq,Ord)
  
data Expr a
  = Lit (Lit a)
  | This a
  | VarRef a Id
  | DotRef a (Expr a) Id
  | BracketRef a (Expr a) {- container -} (Expr a) {- key -}
  | OpExpr a FOp [Expr a]
  | FuncExpr {
      funcExprX :: a,
      funcExprArgs :: [Id],
      funcExprLocals :: [Id],
      funcExprBody ::  Stmt a
    }
  deriving (Show,Data,Typeable,Eq,Ord)

data Stmt a
  = SeqStmt a [Stmt a]
  | EmptyStmt a
  | AssignStmt a Id (Expr a)
  | DeleteStmt a Id {-result-} Id {-to delete-}
  | NewStmt {
      newStmtX :: a,
      newStmtResultId :: Id,
      newStmtConstructorId :: Id,
      newStmtArgs :: [Id]
    }
  | CallStmt {
      callStmtX :: a,
      callStmtResultId :: Id,
      callStmtFunctionId :: Id,
      callStmtArgs :: [Id]
    }
  | MethodCallStmt {
      methodCallStmtX :: a,
      methodCallStmtResultId :: Id,
      methodCallStmtObjectId :: Id,
      methodCallStmtMethodName :: Id,
      methodCallStmtArgs :: [Id]
    }
  | IndirectMethodCallStmt {
      indirectMethodCallStmtX :: a,
      indirectMethodCallStmtResultId :: Id,
      indirectMethodCallStmtObjectId :: Id,
      indirectMethodCallStmtMethodId :: Id,
      indirectMethodCallStmtArgs :: [Id]
    }
  | IfStmt a (Expr a) (Stmt a) (Stmt a)
  | WhileStmt a (Expr a) (Stmt a)
  | ForInStmt a Id (Expr a) (Stmt a)
  | TryStmt a (Stmt a) Id (Stmt a) {- catch clause -} (Stmt a) {- finally -}
  | ThrowStmt a (Expr a)
  | ReturnStmt a (Maybe (Expr a))
  | LabelledStmt a Id (Stmt a)
  | BreakStmt a String
  | ContinueStmt a String
  | SwitchStmt a Id [(Lit a,Stmt a)]
  | EnterStmt a
  | ExitStmt a
  deriving (Data,Typeable,Eq,Ord)  

stmtLabel :: Stmt a -> a
stmtLabel stmt = case stmt of
  (SeqStmt a ss) -> a
  (EmptyStmt a) -> a
  (AssignStmt a v e) -> a
  (DeleteStmt a v1 v2) -> a
  (NewStmt a result constr args) -> a
  (CallStmt a result fn args) -> a
  (MethodCallStmt a result obj method args) -> a
  (IndirectMethodCallStmt a result obj method args) -> a
  (IfStmt a e s1 s2) -> a
  (WhileStmt a e s) -> a
  (ForInStmt a v e s) -> a
  (TryStmt a s1 v s2 s3) -> a
  (ReturnStmt a Nothing) -> a
  (ReturnStmt a (Just e)) -> a
  (LabelledStmt a v s) -> a
  (BreakStmt a v) -> a
  (ContinueStmt a v) -> a
  (SwitchStmt a v cs) -> a
  (EnterStmt a) -> a
  (ExitStmt a) -> a
  ThrowStmt a _ -> a


-- Instances

instance Functor Lit where
  fmap f (StringLit a s) = StringLit (f a) s
  fmap f (RegexpLit a s g ci) = RegexpLit (f a) s g ci
  fmap f (NumLit a d) = NumLit (f a) d
  fmap f (IntLit a n) = IntLit (f a) n
  fmap f (BoolLit a b) = BoolLit (f a) b
  fmap f (NullLit a) = NullLit (f a)
  fmap f (ArrayLit a es) = ArrayLit (f a) (map (fmap f) es)
  fmap f (ObjectLit a es) = ObjectLit (f a) (map (second (fmap f)) es)

instance Functor Expr where
  fmap f (Lit l) = Lit (fmap f l)
  fmap f (This a) = This (f a)
  fmap f (VarRef a v) = VarRef (f a) v
  fmap f (BracketRef a e1 e2) = BracketRef (f a) (fmap f e1) (fmap f e2)
  fmap f (DotRef a e1 m) = DotRef (f a) (fmap f e1) m
  fmap f (OpExpr a op es) = OpExpr (f a) op (map (fmap f) es)
  fmap f (FuncExpr a args locals s) =
    FuncExpr (f a) args locals (fmap f s)

instance Functor Stmt where
  fmap f (SeqStmt a ss) = SeqStmt (f a) (map (fmap f) ss)
  fmap f (EmptyStmt a) = EmptyStmt (f a)
  fmap f (AssignStmt a v e) = AssignStmt (f a) v (fmap f e)
  fmap f (DeleteStmt a v1 v2) = DeleteStmt (f a) v1 v2
  fmap f (NewStmt a result constr args) = NewStmt (f a) result constr args
  fmap f (CallStmt a result fn args) = CallStmt (f a) result fn args
  fmap f (MethodCallStmt a result obj method args) =
    MethodCallStmt (f a) result obj method args
  fmap f (IndirectMethodCallStmt a result obj method args) =
    IndirectMethodCallStmt (f a) result obj method args
  fmap f (IfStmt a e s1 s2) = IfStmt (f a) (fmap f e) (fmap f s1) (fmap f s2)
  fmap f (WhileStmt a e s) = WhileStmt (f a) (fmap f e) (fmap f s)
  fmap f (ForInStmt a v e s) = ForInStmt (f a) v (fmap f e) (fmap f s)
  fmap f (TryStmt a s1 v s2 s3) = 
    TryStmt (f a) (fmap f s1) v (fmap f s2) (fmap f s3)
  fmap f (ReturnStmt a Nothing) = ReturnStmt (f a) Nothing
  fmap f (ReturnStmt a (Just e)) = ReturnStmt (f a) (Just (fmap f e))
  fmap f (LabelledStmt a v s) = LabelledStmt (f a) v (fmap f s)
  fmap f (BreakStmt a v) = BreakStmt (f a) v
  fmap f (ContinueStmt a v) = ContinueStmt (f a) v
  fmap f (SwitchStmt a v cs) =
    SwitchStmt (f a) v (map (fmap f *** fmap f) cs)
  fmap f (EnterStmt a) = EnterStmt (f a)
  fmap f (ExitStmt a) = ExitStmt (f a)
  fmap f (ThrowStmt a e) = ThrowStmt (f a) (fmap f e)

instance Show (Stmt a) where
  show stmt = case stmt of
    SeqStmt a ss -> "Seq ..."
    EmptyStmt a -> "No-op ..."
    AssignStmt a v e -> v ++ " := ..."
    DeleteStmt a v1 v2 -> printf "%s := delete ..." v1
    NewStmt a result constr args ->
      printf "%s := new %s (...)" result constr
    CallStmt a result fn args -> 
      printf "%s := %s (...)" result fn
    MethodCallStmt a result obj method args ->
      printf "%s := %s.%s(...)" result obj method
    IndirectMethodCallStmt a result obj method args ->
      printf "%s := %s[%s](...)" result obj method
    IfStmt a e s1 s2 -> "if ..."
    WhileStmt a e s -> "while ..."
    ForInStmt a v e s -> printf "for (%s in ..." v
    TryStmt a s1 v s2 s3 -> "try ..."
    ReturnStmt a Nothing -> "return;"
    ReturnStmt a (Just e) -> "return ...;" 
    LabelledStmt a v s -> printf "Label %s" v
    BreakStmt a v -> printf "break %s" v
    ContinueStmt a v -> printf "continue %s" v
    SwitchStmt a v cs -> "switch ..."
    EnterStmt a -> "ENTER"
    ExitStmt a -> "EXIT"
    ThrowStmt _ _ -> "throw ..."
