-- | A few helpers to work with the AST annotations
module Language.ECMAScript5.Syntax.Annotations where

import Language.ECMAScript5.Syntax
import Data.Traversable
import Control.Applicative
import Control.Arrow
import Control.Monad.State hiding (mapM)
import Prelude hiding (mapM)

-- | Removes annotations from a tree
removeAnnotations :: Traversable t => t a -> t ()
removeAnnotations = reannotate (const ())

-- | Changes all the labels in the tree to another one, given by a
-- function.
reannotate :: Traversable t => (a -> b) -> t a -> t b
reannotate f tree = traverse (pure . f) tree ()

-- | add an extra field to the AST labels (the label would look like @
-- (a, b) @)
addExtraAnnotationField :: Traversable t => b -> t a -> t (a, b)
addExtraAnnotationField def t = traverse (\z -> pure (z, def)) t ()

-- | remove an extra field
removeExtraAnnotationField :: Traversable t => t (a, b) -> t a
removeExtraAnnotationField t = traverse (pure . fst) t ()


-- | Assigns unique numeric (Int) ids to each node in the AST. Returns
-- a pair: the tree annotated with UID's and the last ID that was
-- assigned.
assignUniqueIds :: Traversable t => Int -- ^ starting id
                                 -> t a -- ^ tree root
                                 -> (t (a, Int), Int) 
assignUniqueIds first tree =
  (returnA *** \i -> i-1) $ runState (mapM f tree) first
  where f :: a -> State Int (a, Int)
        f a = do i <- get
                 put (i+1)
                 return (a, i)

class HasAnnotation a where
  -- | Returns the annotation of the root of the tree
  getAnnotation :: a b -> b

instance HasAnnotation Expression where
  getAnnotation e = case e of
   (StringLit a s)              -> a
   (RegexpLit a s g ci)         -> a
   (NumLit a d)                 -> a
   (IntLit a i)                 -> a
   (BoolLit a b)                -> a
   (NullLit a)                  -> a
   (ArrayLit a exps)            -> a
   (ObjectLit a props)          -> a
   (ThisRef a)                  -> a
   (VarRef a id)                -> a
   (DotRef a exp id)            -> a
   (BracketRef a container key) -> a
   (NewExpr a ctor params)      -> a
   (PrefixExpr a op e)          -> a
   (UnaryAssignExpr a op lv)    -> a
   (InfixExpr a op e1 e2)       -> a
   (CondExpr a g et ef)         -> a
   (AssignExpr a op lv e)       -> a
   (ParenExpr a e)              -> a
   (ListExpr a es)              -> a
   (CallExpr a fn params)       -> a
   (FuncExpr a mid args s)      -> a

instance HasAnnotation Statement where
  getAnnotation s = case s of
    BlockStmt a _        -> a
    EmptyStmt a          -> a
    ExprStmt a _         -> a
    IfStmt a _ _ _       -> a
    IfSingleStmt a _ _   -> a
    SwitchStmt a _ _     -> a
    WhileStmt a _ _      -> a
    DoWhileStmt a _ _    -> a
    BreakStmt a _        -> a
    ContinueStmt a _     -> a
    LabelledStmt a _ _   -> a
    ForInStmt a _ _ _    -> a
    ForStmt a _ _ _ _    -> a
    TryStmt a _ _ _      -> a
    ThrowStmt a _        -> a
    ReturnStmt a _       -> a
    WithStmt a _ _       -> a
    VarDeclStmt a _      -> a
    FunctionStmt a _ _ _ -> a
    
instance HasAnnotation LValue where
  getAnnotation lv = case lv of
    LVar a _ -> a
    LDot a _ _ -> a
    LBracket a _ _ -> a
  
instance HasAnnotation VarDecl where
  getAnnotation (VarDecl a _ _) = a

instance HasAnnotation Prop  where
  getAnnotation p = case p of
    PropId a _ -> a
    PropString a _ -> a
    PropNum a _ -> a
  
instance HasAnnotation CaseClause where
  getAnnotation c = case c of
    CaseClause a _ _ -> a
    CaseDefault a _ -> a
    
instance HasAnnotation CatchClause where
  getAnnotation (CatchClause a _ _) = a
