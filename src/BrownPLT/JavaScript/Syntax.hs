-- |JavaScript's syntax.
module BrownPLT.JavaScript.Syntax(Expression(..),CaseClause(..),Statement(..),
         InfixOp(..),CatchClause(..),VarDecl(..),JavaScript(..),
         AssignOp(..),Id(..),PrefixOp(..),Prop(..),
         ForInit(..),ForInInit(..),unId
  , UnaryAssignOp (..)
  , LValue (..)
  ) where

import Text.ParserCombinators.Parsec(SourcePos) -- used by data JavaScript
import Data.Generics(Data,Typeable)

data JavaScript a
  -- |A script in <script> ... </script> tags.  This may seem a little silly,
  -- but the Flapjax analogue has an inline variant and attribute-inline 
  -- variant.
  = Script a [Statement a] 
  deriving (Show,Data,Typeable,Eq,Ord)

data Id a = Id a String deriving (Show,Eq,Ord,Data,Typeable)

unId :: Id a -> String
unId (Id _ s) = s

-- http://developer.mozilla.org/en/docs/
--   Core_JavaScript_1.5_Reference:Operators:Operator_Precedence
data InfixOp = OpLT | OpLEq | OpGT | OpGEq  | OpIn  | OpInstanceof | OpEq | OpNEq
             | OpStrictEq | OpStrictNEq | OpLAnd | OpLOr 
             | OpMul | OpDiv | OpMod  | OpSub | OpLShift | OpSpRShift
             | OpZfRShift | OpBAnd | OpBXor | OpBOr | OpAdd
    deriving (Show,Data,Typeable,Eq,Ord,Enum)

data AssignOp = OpAssign | OpAssignAdd | OpAssignSub | OpAssignMul | OpAssignDiv
  | OpAssignMod | OpAssignLShift | OpAssignSpRShift | OpAssignZfRShift
  | OpAssignBAnd | OpAssignBXor | OpAssignBOr
  deriving (Show,Data,Typeable,Eq,Ord)

data UnaryAssignOp
  = PrefixInc | PrefixDec | PostfixInc | PostfixDec
  deriving (Show, Data, Typeable, Eq, Ord)

data PrefixOp = PrefixLNot | PrefixBNot | PrefixPlus
  | PrefixMinus | PrefixTypeof | PrefixVoid | PrefixDelete
  deriving (Show,Data,Typeable,Eq,Ord)
  
data Prop a 
  = PropId a (Id a) | PropString a String | PropNum a Integer
  deriving (Show,Data,Typeable,Eq,Ord)
 
data LValue a
  = LVar a String
  | LDot a (Expression a) String
  | LBracket a (Expression a) (Expression a)
  deriving (Show, Eq, Ord, Data, Typeable) 

data Expression a
  = StringLit a String
  | RegexpLit a String Bool {- global? -} Bool {- case-insensitive? -}
  | NumLit a Double
  | IntLit a Int
  | BoolLit a Bool
  | NullLit a
  | ArrayLit a [Expression a]
  | ObjectLit a [(Prop a, Expression a)]
  | ThisRef a
  | VarRef a (Id a)
  | DotRef a (Expression a) (Id a)
  | BracketRef a (Expression a) {- container -} (Expression a) {- key -}
  | NewExpr a (Expression a) {- constructor -} [Expression a]
  | PrefixExpr a PrefixOp (Expression a)
  | UnaryAssignExpr a UnaryAssignOp (LValue a)
  | InfixExpr a InfixOp (Expression a) (Expression a)
  | CondExpr a (Expression a) (Expression a) (Expression a)
  | AssignExpr a AssignOp (LValue a) (Expression a)
  | ParenExpr a (Expression a)
  | ListExpr a [Expression a]
  | CallExpr a (Expression a) [Expression a]
  | FuncExpr a [(Id a)] (Statement a)
  deriving (Show,Data,Typeable,Eq,Ord)

data CaseClause a 
  = CaseClause a (Expression a) [Statement a]
  | CaseDefault a [Statement a]
  deriving (Show,Data,Typeable,Eq,Ord)
  
data CatchClause a 
  = CatchClause a (Id a) (Statement a) 
  deriving (Show,Data,Typeable,Eq,Ord)

data VarDecl a 
  = VarDecl a (Id a) (Maybe (Expression a)) 
  deriving (Show,Data,Typeable,Eq,Ord)
  
data ForInit a
  = NoInit
  | VarInit [VarDecl a]
  | ExprInit (Expression a)
  deriving (Show,Data,Typeable,Eq,Ord)

data ForInInit a
 = ForInVar (Id a)
 | ForInNoVar (Id a)
 deriving (Show,Data,Typeable,Eq,Ord)
  
  
data Statement a
  = BlockStmt a [Statement a]
  | EmptyStmt a
  | ExprStmt a (Expression a)
  | IfStmt a (Expression a) (Statement a) (Statement a)
  | IfSingleStmt a (Expression a) (Statement a)
  | SwitchStmt a (Expression a) [CaseClause a]
  | WhileStmt a (Expression a) (Statement a)
  | DoWhileStmt a (Statement a) (Expression a)
  | BreakStmt a (Maybe (Id a))
  | ContinueStmt a (Maybe (Id a))
  | LabelledStmt a (Id a) (Statement a)
  | ForInStmt a (ForInInit a) (Expression a) (Statement a)
  | ForStmt a (ForInit a)        
              (Maybe (Expression a)) -- increment
              (Maybe (Expression a)) -- test
              (Statement a)          -- body
  | TryStmt a (Statement a) {-body-} [CatchClause a] {-catches-}
      (Maybe (Statement a)) {-finally-}
  | ThrowStmt a (Expression a)
  | ReturnStmt a (Maybe (Expression a))
  | WithStmt a (Expression a) (Statement a)
  | VarDeclStmt a [VarDecl a]
  | FunctionStmt a (Id a) {-name-} [(Id a)] {-args-} (Statement a) {-body-}
  deriving (Show,Data,Typeable,Eq,Ord)  
