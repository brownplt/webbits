-- |Instances of 'Foldable' and 'Traversable' for JavaScript's syntax.
module BrownPLT.JavaScript.Instances
  ( -- This module does not export any names.
  ) where

import Prelude hiding (foldr,sequence,mapM)
import qualified Prelude as Prelude

import Data.Foldable (Foldable(..))
import Data.Traversable(Traversable(..))
import Control.Applicative

import BrownPLT.JavaScript.Syntax

yfoldr:: Foldable t => (a -> b -> b) -> b -> Maybe (t a) -> b
yfoldr _ b Nothing = b
yfoldr f b (Just t) = foldr f b t

lfoldr:: Foldable t => (a -> b -> b) -> b -> [t a] -> b
lfoldr f = Prelude.foldr (flip $ foldr f)

ltraverse:: (Traversable t, Applicative f) => (a -> f b) -> [t a] -> f [t b]
ltraverse _ [] = pure []
ltraverse f (a:as) = (:) <$> traverse f a <*> ltraverse f as

ytraverse:: (Traversable t, Applicative f) 
         => (a -> f b) -> Maybe (t a) -> f (Maybe (t b))
ytraverse _ Nothing = pure Nothing
ytraverse f (Just t) = Just <$> traverse f t

instance Functor Id where
  fmap f (Id a v) = Id (f a) v

instance Functor JavaScript where
  fmap f (Script a stmts) = Script (f a) (map (fmap f) stmts)
  
instance Functor Prop where
  fmap f (PropId a id) = PropId (f a) (fmap f id)
  fmap f (PropString a s) = PropString (f a) s
  fmap f (PropNum a n) = PropNum (f a) n

instance Functor LValue where
  fmap f lv = case lv of
    LVar a x -> LVar (f a) x
    LDot a e x -> LDot (f a) (fmap f e) x
    LBracket a e1 e2 -> LBracket (f a) (fmap f e1) (fmap f e2) 

instance Functor Expression where
  fmap f expression = 
    case expression of
      StringLit a s -> StringLit (f a) s
      RegexpLit a s g ci -> RegexpLit (f a) s g ci
      NumLit a n -> NumLit (f a) n
      IntLit a n -> IntLit (f a) n
      BoolLit a b -> BoolLit (f a) b
      NullLit a -> NullLit (f a)
      ArrayLit a es -> ArrayLit (f a) (map (fmap f) es)
      ObjectLit a pes -> ObjectLit (f a) (map f' pes) where
        f' (p,e) = (fmap f p, fmap f e)
      ThisRef a -> ThisRef (f a)
      VarRef a id -> VarRef (f a) (fmap f id)
      DotRef a e id -> DotRef (f a) (fmap f e) (fmap f id)
      BracketRef a e1 e2 -> BracketRef (f a) (fmap f e1) (fmap f e2)
      NewExpr a e es -> NewExpr (f a) (fmap f e) (map (fmap f) es)
      PrefixExpr a op e -> PrefixExpr (f a) op (fmap f e)
      InfixExpr a op e1 e2 -> InfixExpr (f a) op (fmap f e1) (fmap f e2)
      CondExpr a e1 e2 e3 -> CondExpr (f a) (fmap f e1) (fmap f e2) (fmap f e3)
      AssignExpr a op e1 e2 -> AssignExpr (f a) op (fmap f e1) (fmap f e2)
      UnaryAssignExpr a op e -> UnaryAssignExpr (f a) op (fmap f e)
      ParenExpr a e -> ParenExpr (f a) (fmap f e)
      ListExpr a es -> ListExpr (f a) (map (fmap f) es)
      CallExpr a e es -> CallExpr (f a) (fmap f e) (map (fmap f) es)
      FuncExpr a args s -> FuncExpr (f a) (map (fmap f) args) (fmap f s)

instance Functor CaseClause where
  fmap f (CaseClause a e ss) = CaseClause (f a) (fmap f e) (map (fmap f) ss)
  fmap f (CaseDefault a ss) = CaseDefault (f a) (map (fmap f) ss)
  
instance Functor CatchClause where
  fmap f (CatchClause a id s) = CatchClause (f a) (fmap f id) (fmap f s)

instance Functor VarDecl where
  fmap f (VarDecl a id Nothing) = VarDecl (f a) (fmap f id) Nothing
  fmap f (VarDecl a id (Just e)) = VarDecl (f a) (fmap f id) (Just $ fmap f e)
  
instance Functor ForInit where
  fmap f NoInit = NoInit
  fmap f (VarInit decls) = VarInit (map (fmap f) decls)
  fmap f (ExprInit e) = ExprInit (fmap f e)

instance Functor ForInInit where
  fmap f (ForInVar id) = ForInVar (fmap f id)
  fmap f (ForInNoVar id) = ForInNoVar (fmap f id)
  
instance Functor Statement where
  fmap f s = 
    case s of
      BlockStmt a ss -> BlockStmt (f a) (map (fmap f) ss)
      EmptyStmt a -> EmptyStmt (f a)
      ExprStmt a e -> ExprStmt (f a) (fmap f e)
      IfStmt a e s1 s2 -> IfStmt (f a) (fmap f e) (fmap f s1) (fmap f s2)
      IfSingleStmt a e s -> IfSingleStmt (f a) (fmap f e) (fmap f s)
      SwitchStmt a e cs -> SwitchStmt (f a) (fmap f e) (map (fmap f) cs)
      WhileStmt a e s -> WhileStmt (f a) (fmap f e) (fmap f s)
      DoWhileStmt a s e -> DoWhileStmt (f a) (fmap f s) (fmap f e)
      BreakStmt a Nothing -> BreakStmt (f a) Nothing
      BreakStmt a (Just id) -> BreakStmt (f a) (Just (fmap f id))
      ContinueStmt a yid -> ContinueStmt (f a) (yid >>= return.(fmap f))
      LabelledStmt a id s -> LabelledStmt (f a) (fmap f id) (fmap f s)
      ForInStmt a init e s -> ForInStmt (f a) (fmap f init) (fmap f e)
                                (fmap f s)
      ForStmt a init yinc ytest body -> 
        ForStmt (f a) (fmap f init) (yinc >>= return.(fmap f))
          (ytest >>= return.(fmap f)) (fmap f body)
      TryStmt a s cs ys -> 
        TryStmt (f a) (fmap f s) (map (fmap f) cs) (ys >>= return.(fmap f))
      ThrowStmt a e -> ThrowStmt (f a) (fmap f e)
      ReturnStmt a ye -> ReturnStmt (f a) (ye >>= return.(fmap f))
      WithStmt a e s -> WithStmt (f a) (fmap f e) (fmap f s)
      VarDeclStmt a ds -> VarDeclStmt (f a) (map (fmap f) ds)
      FunctionStmt a id args s -> 
        FunctionStmt (f a) (fmap f id) (map (fmap f) args) (fmap f s)

instance Foldable Id where
  foldr f b (Id a _) = f a b

instance Foldable Prop where
  foldr f b (PropId a id) = f a (foldr f b id)
  foldr f b (PropString a _) = f a b
  foldr f b (PropNum a _) = f a b

instance Foldable LValue where
  foldr f b (LVar a x) = f a b
  foldr f b (LDot a e x) = f a (foldr f b e)
  foldr f b (LBracket a e1 e2) = f a (foldr f (foldr f b e2) e1)

        
instance Foldable Expression where
  -- foldr:: (a -> b -> b) -> b -> Expression a -> b
  foldr f b e =
    case e of
      StringLit a _ -> f a b
      RegexpLit a _ _ _ -> f a b
      NumLit a _ -> f a b
      IntLit a _ -> f a b
      BoolLit a _ -> f a b
      NullLit a -> f a b
      ArrayLit a es -> f a (Prelude.foldr (flip $ foldr f) b es)
      ObjectLit a pes -> f a (Prelude.foldr f' b pes) where
        f' (p,e) b = foldr f (foldr f b e) p
      ThisRef a -> f a b
      VarRef a id -> f a (foldr f b id)
      DotRef a e id -> f a (foldr f (foldr f b id) e)
      BracketRef a e1 e2 -> f a (foldr f (foldr f b e2) e1)
      NewExpr a e es -> f a (foldr f (Prelude.foldr (flip $ foldr f) b es) e)
      PrefixExpr a _ e -> f a $ foldr f b e
      InfixExpr a _ e1 e2 -> f a $ foldr f (foldr f b e2) e1
      CondExpr a e1 e2 e3 -> f a $ foldr f (foldr f (foldr f b e3) e2) e1
      AssignExpr a _ e1 e2 -> f a $ foldr f (foldr f b e2) e1
      UnaryAssignExpr a _ lv -> f a (foldr f b lv)
      ParenExpr a e -> f a $ foldr f b e
      ListExpr a es -> f a $ Prelude.foldr (flip $ foldr f) b es
      CallExpr a e es -> f a $ foldr f (Prelude.foldr (flip $ foldr f) b es) e
      FuncExpr a args s -> 
        f a $ Prelude.foldr (flip $ foldr f) (foldr f b s) args

instance Foldable CaseClause where
  foldr f b (CaseClause a e ss) = 
    f a $ foldr f (Prelude.foldr (flip $ foldr f) b ss) e
  foldr f b (CaseDefault a ss) = f a $ Prelude.foldr (flip $ foldr f) b ss
  
instance Foldable CatchClause where
  foldr f b (CatchClause a id s) = f a $ foldr f (foldr f b s) id

instance Foldable VarDecl where
  foldr f b (VarDecl a id ye) = f a $ foldr f (yfoldr f b ye) id

instance Foldable ForInit where
  foldr f b NoInit = b
  foldr f b (VarInit ds) = Prelude.foldr (flip $ foldr f) b ds
  foldr f b (ExprInit e) = foldr f b e

instance Foldable ForInInit where
 foldr f b (ForInVar id) = foldr f b id
 foldr f b (ForInNoVar id) = foldr f b id

instance Foldable Statement where
  foldr f b statement =
    case statement of
      BlockStmt a ss -> f a $ lfoldr f b ss
      EmptyStmt a -> f a b
      ExprStmt a e -> f a $ foldr f b e
      IfStmt a e s1 s2 -> f a $ foldr f (foldr f (foldr f b s2) s1) e
      IfSingleStmt a e s -> f a $ foldr f (foldr f b s) e
      SwitchStmt a e cs -> f a $ foldr f (lfoldr f b cs) e
      WhileStmt a e s -> f a $ foldr f (foldr f b s) e
      DoWhileStmt a s e -> f a $ foldr f (foldr f b e) s
      BreakStmt a yid -> f a $ yfoldr f b yid
      ContinueStmt a yid -> f a $ yfoldr f b yid
      LabelledStmt a id s -> f a $ foldr f (foldr f b s) id
      ForInStmt a init e s -> f a $ foldr f (foldr f (foldr f b s) e) init
      ForStmt a init ye1 ye2 s ->
        f a $ foldr f (yfoldr f (yfoldr f (foldr f b s) ye2) ye1) init
      TryStmt a s cs ys -> f a $ foldr f (lfoldr f (yfoldr f b ys) cs) s
      ThrowStmt a e -> f a $ foldr f b e
      ReturnStmt a ys -> f a $ yfoldr f b ys
      WithStmt a e s -> f a $ foldr f (foldr f b s) e
      VarDeclStmt a ds -> f a $ lfoldr f b ds
      FunctionStmt a id args s ->
        f a $ lfoldr f (foldr f b s) (id:args)

instance Traversable Id where
  traverse f (Id a v) = Id <$> f a <*> pure v

instance Traversable Prop where
  traverse f (PropId a id) = PropId <$> f a <*> traverse f id
  traverse f (PropString a s) = PropString <$> f a <*> pure s
  traverse f (PropNum a n) = PropNum <$> f a <*> pure n

instance Traversable LValue where
  traverse f lv = case lv of
    LVar a x -> LVar <$> f a <*> pure x
    LDot a e x -> LDot <$> f a <*> traverse f e <*> pure x
    LBracket a e1 e2 -> LBracket <$> f a <*> traverse f e1 <*> traverse f e2
  
instance Traversable Expression where
  traverse f expression =
    case expression of
      StringLit a s -> StringLit <$> f a <*> pure s
      RegexpLit a s g ci -> RegexpLit <$> f a <*> pure s <*> pure g <*> pure ci
      NumLit a n -> NumLit <$> f a <*> pure n
      IntLit a n -> IntLit <$> f a <*> pure n
      BoolLit a b -> BoolLit <$> f a <*> pure b
      NullLit a -> NullLit <$> f a
      ArrayLit a es -> ArrayLit <$> f a <*> ltraverse f es
      ObjectLit a ps -> ObjectLit <$> f a <*>  (zip <$> props' <*> es') where
        (props,es) = unzip ps
        props' = ltraverse f props
        es' = ltraverse f es
      ThisRef a -> ThisRef <$> f a
      VarRef a id -> VarRef <$> f a <*> traverse f id
      DotRef a e id -> DotRef <$> f a <*> traverse f e <*> traverse f id
      BracketRef a e es -> BracketRef <$> f a <*> traverse f e <*> traverse f es
      NewExpr a e es -> NewExpr <$> f a <*> traverse f e <*> ltraverse f es
      PrefixExpr a op e -> PrefixExpr <$> f a <*> pure op <*> traverse f e
      InfixExpr a op e1 e2 -> InfixExpr <$> f a <*> pure op <*> traverse f e1
                                <*> traverse f e2
      CondExpr a e1 e2 e3 ->
        CondExpr <$> f a <*> traverse f e1 <*> traverse f e2 <*> traverse f e3
      AssignExpr a op e1 e2 -> AssignExpr <$> f a <*> pure op <*> traverse f e1
        <*> traverse f e2
      UnaryAssignExpr a op e ->
        UnaryAssignExpr <$> f a <*> pure op <*> traverse f e
      ParenExpr a e -> ParenExpr <$> f a <*> traverse f e
      ListExpr a es -> ListExpr <$> f a <*> ltraverse f es
      CallExpr a e es -> CallExpr <$> f a <*> traverse f e <*> ltraverse f es
      FuncExpr a ids s -> FuncExpr <$> f a <*> ltraverse f ids <*> traverse f s

instance Traversable CaseClause where
  traverse f (CaseClause a e ss) =
    CaseClause <$> f a <*> traverse f e <*> ltraverse f ss
  traverse f (CaseDefault a ss) = 
    CaseDefault <$> f a <*> ltraverse f ss

instance Traversable CatchClause where
  traverse f (CatchClause a id s) = 
    CatchClause <$> f a <*> traverse f id <*> traverse f s
    
instance Traversable VarDecl where
  traverse f (VarDecl a id ye) =
    VarDecl <$> f a <*> traverse f id <*> ytraverse f ye

instance Traversable ForInit where
  traverse f NoInit = pure NoInit
  traverse f (VarInit ds) = VarInit <$> ltraverse f ds
  traverse f (ExprInit e) = ExprInit <$> traverse f e

instance Traversable ForInInit where
 traverse f (ForInVar id) = ForInVar <$> traverse f id
 traverse f (ForInNoVar id) = ForInNoVar <$> traverse f id

instance Traversable Statement where
  traverse f statement =
    case statement of
      BlockStmt a ss -> BlockStmt <$> f a <*> ltraverse f ss
      EmptyStmt a -> EmptyStmt <$> f a
      ExprStmt a e -> ExprStmt <$> f a <*> traverse f e
      IfStmt a e s1 s2 ->
        IfStmt <$> f a <*> traverse f e <*> traverse f s1 <*> traverse f s2
      IfSingleStmt a e s ->
        IfSingleStmt <$> f a <*> traverse f e <*> traverse f s
      SwitchStmt a e cs ->
        SwitchStmt <$> f a <*> traverse f e <*> ltraverse f cs
      WhileStmt a e s -> WhileStmt <$> f a <*> traverse f e <*> traverse f s
      DoWhileStmt a s e -> DoWhileStmt <$> f a <*> traverse f s <*> traverse f e
      BreakStmt a yid -> BreakStmt <$> f a <*> ytraverse f yid
      ContinueStmt a yid -> ContinueStmt <$> f a <*> ytraverse f yid
      LabelledStmt a id s -> 
        LabelledStmt <$> f a <*> traverse f id <*> traverse f s
      ForInStmt a init e s ->
        ForInStmt <$> f a <*> traverse f init <*> traverse f e <*> traverse f s
      ForStmt a init yinc ytest s ->
        ForStmt <$> f a <*> traverse f init <*> ytraverse f yinc 
          <*> ytraverse f ytest <*> traverse f s
      TryStmt a s cs ys ->
        TryStmt <$> f a <*> traverse f s <*> ltraverse f cs <*> ytraverse f ys
      ThrowStmt a e -> ThrowStmt <$> f a <*> traverse f e
      ReturnStmt a ys -> ReturnStmt <$> f a <*> ytraverse f ys
      WithStmt a e s -> WithStmt <$> f a <*> traverse f e <*> traverse f s
      VarDeclStmt a ds -> VarDeclStmt <$> f a <*> ltraverse f ds
      FunctionStmt a id args s ->
        FunctionStmt <$> f a <*> traverse f id <*> ltraverse f args 
          <*> traverse f s 
