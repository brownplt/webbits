{-# LANGUAGE TemplateHaskell #-}
-- | QuickCheck $Arbitrary$ instances for ECMAScript 3 abstract
-- syntax.

module Language.ECMAScript3.Syntax.Arbitrary where

import Language.ECMAScript3.Syntax
import Test.QuickCheck hiding (Prop)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property (forAllShrink)
import Data.Map hiding (map,null,filter,foldr)
import Data.List (nub,delete)
import Data.Data
import Data.Char
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Generics.Str
import Control.Monad
import Control.Monad.State
import Data.Maybe (maybeToList)

instance Arbitrary (AssignOp) where
  arbitrary = 
    elements [OpAssign, OpAssignAdd, OpAssignSub, OpAssignMul, OpAssignDiv, 
              OpAssignMod, OpAssignLShift, OpAssignSpRShift, OpAssignZfRShift,
              OpAssignBAnd, OpAssignBXor, OpAssignBOr]

instance Arbitrary (InfixOp) where
  arbitrary = 
    elements [OpLT, OpLEq, OpGT, OpGEq , OpIn , OpInstanceof, OpEq, OpNEq, 
              OpStrictEq, OpStrictNEq, OpLAnd, OpLOr,
              OpMul, OpDiv, OpMod , OpSub, OpLShift, OpSpRShift,
              OpZfRShift, OpBAnd, OpBXor, OpBOr, OpAdd]
  
instance Arbitrary (UnaryAssignOp) where
  arbitrary = 
    elements [PrefixInc, PrefixDec, PostfixInc, PostfixDec]
  
instance Arbitrary (PrefixOp) where
  arbitrary = 
    elements [PrefixLNot, PrefixBNot, PrefixPlus, PrefixMinus, 
              PrefixTypeof, PrefixVoid, PrefixDelete]


instance Arbitrary a => Arbitrary (Id a) where
  arbitrary = liftM2 Id arbitrary (sized sizedIdent)
  
    where sizedIdent n = do s <- identStart
                            rest <- identRest (n-1)
                            return (s:rest)
          identStart = arbitrary `suchThat` isIdentStart
          identRest n | n < 1 = return ""
          identRest n = do p <- identPart
                           rest <- identRest (n-1)
                           return (p:rest)
          identPart = do arbitrary `suchThat` isIdentPart
          isIdentStart c = isLetter c || c == '$' || c == '_'
          isIdentPart c = isIdentStart c || isMark c || isNumber c
  shrink (Id a s) = [Id na ns | ns <- shrink s, na <- shrink a]

instance Arbitrary a => Arbitrary (CaseClause a) where
  arbitrary = oneof [caseclause, casedefault]
    where caseclause = liftM3 CaseClause arbitrary arbitrary arbitrary
          casedefault = liftM2 CaseDefault arbitrary arbitrary
  shrink (CaseClause a expr stmts) = 
    [CaseClause na ne ns | na <- shrink a, ne <- shrink expr, ns <- shrink stmts]
  shrink (CaseDefault a stmts) = 
    [CaseDefault na ns | na <- shrink a, ns <- shrink stmts]
                         
instance Arbitrary a => Arbitrary (Prop a) where
  arbitrary = oneof [liftM2 PropId arbitrary arbitrary,
                     liftM2 PropString arbitrary nonEmptyString,
                     liftM2 PropNum arbitrary nonNegative
                    ]
  shrink (PropId a id) = [PropId na nid | nid <- shrink id, na <- shrink a] 
  shrink (PropString a s) = [PropString na ns | ns <- shrink s, na <- shrink a] 
  shrink (PropNum a i) = [PropNum na ni | ni <- shrink i, na <- shrink a] 
  
instance Arbitrary a => Arbitrary (LValue a) where  
  arbitrary = oneof [liftM2 LVar arbitrary arbitrary,
                     liftM3 LDot arbitrary arbitrary arbitrary,
                     liftM3 LBracket arbitrary arbitrary arbitrary]
  shrink (LVar a s) = [LVar na ns | ns <- shrink s, na <- shrink a]
  shrink (LDot a e s) = [LDot na ne ns | ne <- shrink e, ns <-shrink s, na <-shrink a]
  shrink (LBracket a e1 e2) = [LBracket na ne1 ne2 | ne1 <- shrink e1, ne2 <-shrink e2, na <- shrink a]
  
cshrink :: Arbitrary a => [a] -> [a]
cshrink = concat . shrink
          
-- minimum size generator
type MSGen a = (Int, Gen a)

sGen :: [MSGen a] -> Gen a
sGen gens = 
  sized f 
  where f n | n >= 0 = oneof $ map snd (filter (\(m, _) -> n >= m) gens)
        f _          = f 0

recursive :: Gen a -> Gen a
recursive g = sized (\n -> resize (n-1) g)

rarbitrary :: Arbitrary a => Gen a
rarbitrary = recursive arbitrary

rrarbitrary :: Arbitrary a => Gen a
rrarbitrary = recursive $ recursive arbitrary

atLeastOfSize :: Arbitrary a => Int -> Gen a -> Gen a
atLeastOfSize l gen = sized $ \s -> if s < l then resize l gen else gen

nonEmptyString :: Gen String
nonEmptyString = sized $ \s -> if s < 1 then stringOfLength 1 else stringOfLength s

nonNegative :: (Arbitrary a, Num a) => Gen a
nonNegative = liftM abs arbitrary

stringOfLength :: Int -> Gen String
stringOfLength 0 = return ""
stringOfLength n = do c <- arbitrary
                      rs <- stringOfLength (n-1)
                      return (c:rs)

instance Arbitrary a => Arbitrary (Expression a) where
  arbitrary = 
    sGen [(0, liftM  ThisRef arbitrary),
          (0, liftM  NullLit arbitrary),
          (0, liftM2 StringLit arbitrary arbitrary),
          (0, liftM2 NumLit arbitrary nonNegative),
          (0, liftM2 IntLit arbitrary nonNegative),
          (0, liftM2 BoolLit arbitrary arbitrary),
          (0, liftM4 RegexpLit arbitrary nonEmptyString arbitrary arbitrary),
          (1, liftM2 ArrayLit arbitrary rarbitrary),
          (1, liftM2 ObjectLit arbitrary rarbitrary),
          (0, liftM2 VarRef arbitrary arbitrary),
          (1, liftM3 DotRef arbitrary rarbitrary arbitrary),
          (2, liftM3 BracketRef arbitrary rarbitrary rarbitrary),
          (3, liftM3 NewExpr arbitrary rarbitrary rrarbitrary),
          (1, liftM3 PrefixExpr arbitrary arbitrary rarbitrary),
          (2, liftM3 UnaryAssignExpr arbitrary arbitrary rarbitrary),
          (2, liftM4 InfixExpr arbitrary arbitrary rarbitrary rarbitrary),
          (3, liftM4 CondExpr arbitrary rarbitrary rarbitrary rarbitrary),
          (3, liftM4 AssignExpr arbitrary rarbitrary rarbitrary rarbitrary),
          (3, liftM2 ListExpr arbitrary (recursive (atLeastOfSize 2 arbitrary))),
          (3, liftM3 CallExpr arbitrary rarbitrary rrarbitrary),
          (1, liftM4 FuncExpr arbitrary arbitrary arbitrary rarbitrary)]
    
  shrink (StringLit a s) = [StringLit na ns | na <- shrink a, ns <- shrink s]
  shrink (RegexpLit a s b1 b2) = [RegexpLit na ns nb1 nb2 | na <- shrink a, nb1 <- shrink b1, nb2 <- shrink b2, ns <- shrink s]
  shrink (NumLit a d) = [NumLit na nd | na <- shrink a, nd <- shrink d]
  shrink (IntLit a i) = [IntLit na ni | na <- shrink a, ni <- shrink i]
  shrink (BoolLit a b) = [BoolLit na nb | na <- shrink a, nb <- shrink b]
  shrink (NullLit a) = [NullLit na | na <- shrink a]
  shrink (ArrayLit a xs) = (cshrink xs) ++ xs ++ [ArrayLit na nxs | na <- shrink a, nxs <- shrink xs]
  shrink (ObjectLit a xs) =  
    let es = map snd xs in
    (cshrink es) ++ es ++
    [ObjectLit na nxs | na <- shrink a, nxs <- shrink xs]
  shrink (ThisRef a) = [ThisRef na | na <- shrink a]
  shrink (VarRef a id) = [VarRef na nid | na <- shrink a, nid <- shrink id]
  shrink (DotRef a e id) = [DotRef na ne nid | na <-shrink a, nid <- shrink id,  ne <- shrink e]
  shrink (BracketRef a o k) = [BracketRef na no nk | na <- shrink a, no <-shrink o, nk <- shrink k]
  shrink (NewExpr a c xs) = (shrink c) ++ [c] ++ (cshrink xs) ++ xs ++ [NewExpr na nc nxs | na <- shrink a, nc <- shrink c,  nxs <- shrink xs]
  shrink (PrefixExpr a op e) = (shrink e) ++ [e] ++ [PrefixExpr na nop ne | na <- shrink a, nop <-shrink op, ne <- shrink e]
  shrink (UnaryAssignExpr a op v) = [UnaryAssignExpr na nop nv | na <- shrink a, nop <- shrink op, nv <- shrink v]
  shrink (InfixExpr a op e1 e2) = (shrink e1) ++ [e1] ++ (shrink e2) ++ [e2] ++ [InfixExpr na nop ne1 ne2 | na <- shrink a, nop <- shrink op, ne1 <- shrink e1, ne2 <- shrink e2]
  shrink (CondExpr a e1 e2 e3) = (shrink e1) ++ [e1] ++ (shrink e2) ++ [e2] ++ (shrink e3) ++ [e3] ++ [CondExpr na ne1 ne2 ne3 | na <- shrink a, ne1 <- shrink e1, ne2 <- shrink e2, ne3 <- shrink e3]
  shrink (AssignExpr a op v e) = (shrink e) ++ [e] ++ [AssignExpr na nop nv ne | na <- shrink a, nop <- shrink op, nv <- shrink v, ne <-shrink e] 
  shrink (ListExpr a es) = (cshrink es) ++ es ++ [ListExpr na nes | na <- shrink a, nes <- shrink es]
  shrink (CallExpr a e es) = (shrink e) ++ [e] ++ (cshrink es) ++ es ++ [CallExpr na ne nes | na <- shrink a, ne <- shrink e, nes <- shrink es]
  shrink (FuncExpr a mid ids s) = [FuncExpr na nmid nids ns | na <- shrink a, nmid <-  shrink mid, nids <- shrink ids, ns <- shrink s]

instance Arbitrary a => Arbitrary (ForInInit a) where
  arbitrary = oneof [liftM ForInVar arbitrary,
                     liftM ForInLVal arbitrary]
  shrink (ForInVar id) = [ForInVar nid | nid <- shrink id]
  shrink (ForInLVal id) = [ForInLVal nid | nid <- shrink id]
  
instance Arbitrary a => Arbitrary (ForInit a) where  
  arbitrary = 
    frequency [
      (2, return NoInit),
      (1, liftM VarInit arbitrary),
      (1, liftM ExprInit arbitrary)]
  shrink (NoInit) = []
  shrink (VarInit vds) = [VarInit nvds | nvds <- shrink vds]
  shrink (ExprInit e) = [ExprInit ne | ne <- shrink e]

instance Arbitrary a => Arbitrary (CatchClause a) where
  arbitrary = liftM3 CatchClause arbitrary arbitrary arbitrary
  shrink (CatchClause a id s) = [CatchClause na nid ns | na <- shrink a, nid <- shrink id, ns <- shrink s]
  
instance Arbitrary a => Arbitrary (VarDecl a) where
  arbitrary = liftM3 VarDecl arbitrary arbitrary arbitrary
  shrink (VarDecl a id me) = [VarDecl na nid nme | na <- shrink a, nid <- shrink id, nme <- shrink me]

instance Arbitrary a => Arbitrary (Statement a) where
  arbitrary = 
    sGen [(2, liftM2 BlockStmt arbitrary rrarbitrary),
          (0, liftM  EmptyStmt arbitrary),
          (1, liftM2 ExprStmt arbitrary rarbitrary),
          (3, liftM4 IfStmt arbitrary rarbitrary rarbitrary rarbitrary),
          (2, liftM3 IfSingleStmt arbitrary rarbitrary rarbitrary),
          (3, liftM3 SwitchStmt arbitrary rarbitrary rrarbitrary),
          (2, liftM3 WhileStmt arbitrary rarbitrary rarbitrary),
          (2, liftM3 DoWhileStmt arbitrary rarbitrary rarbitrary),
          (0, liftM2 BreakStmt arbitrary arbitrary),
          (0, liftM2 ContinueStmt arbitrary arbitrary),
          (1, liftM3 LabelledStmt arbitrary arbitrary rarbitrary),
          (3, liftM4 ForInStmt arbitrary rarbitrary rarbitrary rarbitrary),
          (4, liftM5 ForStmt arbitrary rarbitrary rarbitrary rarbitrary rarbitrary),
          (4, arbtry),
          (1, liftM2 ThrowStmt arbitrary rarbitrary),
          (1, liftM2 ReturnStmt arbitrary rarbitrary),
          (2, liftM3 WithStmt arbitrary rarbitrary rarbitrary),
          (2, liftM2 VarDeclStmt arbitrary rrarbitrary),
          (1, liftM4 FunctionStmt arbitrary arbitrary arbitrary rarbitrary)]
    where arbtry = 
            do (mCatch, mFinally) <- oneof [liftM2 (,) (return Nothing) (liftM Just rarbitrary),
                                            liftM2 (,) (liftM Just rarbitrary) (return Nothing),
                                            liftM2 (,) (liftM Just rarbitrary) (liftM Just rarbitrary)]
               a <- arbitrary                      
               body <- rarbitrary
               return $ TryStmt a body mCatch mFinally
    
  shrink (BlockStmt a body) = emptyStmtShrink a ++ 
                              [BlockStmt as bs | as <- shrink a, bs <- shrink body]
  shrink (EmptyStmt a) = emptyStmtShrink a
  shrink (ExprStmt a e) = emptyStmtShrink a ++ 
                          [ExprStmt as es | as <- shrink a, es <- shrink e]
  shrink (IfStmt a e th el) = emptyStmtShrink a ++
                              [IfStmt as es ths els | as <- shrink a, es <- shrink e, ths <- shrink th, els <- shrink el]
  shrink (IfSingleStmt a e th) = emptyStmtShrink a ++
                                 [IfSingleStmt as es ths | as <- shrink a, es <- shrink e, ths <- shrink th]
  shrink (SwitchStmt a e cases) = emptyStmtShrink a ++
                                  [SwitchStmt as es cs | as <- shrink a, es <-shrink e, cs <- shrink cases] 
  shrink (WhileStmt a e b) = emptyStmtShrink a ++
                             [WhileStmt as es bs | as <- shrink a, es <- shrink e, bs <- shrink b]
  shrink (DoWhileStmt a b e) = emptyStmtShrink a ++  
                               [DoWhileStmt as bs es | as <- shrink a, es <- shrink e, bs <- shrink b]
  shrink (BreakStmt a l) = emptyStmtShrink a ++
                           [BreakStmt as ls | as <- shrink a, ls <- shrink l]
  shrink (ContinueStmt a l) = emptyStmtShrink a ++
                              [ContinueStmt as ls | as <- shrink a, ls <- shrink l]
  shrink (LabelledStmt a l s) = emptyStmtShrink a ++
                                [LabelledStmt as ls ss | as <- shrink a, ls <- shrink l, ss <- shrink s]
  shrink (ForInStmt a i o s) = emptyStmtShrink a ++
                               [ForInStmt as is os ss | as <- shrink a, is <-shrink i, os <-shrink o, ss <- shrink s]
  shrink (ForStmt a i e1 e2 s) = emptyStmtShrink a ++
                                 [ForStmt as is e1s e2s ss | as <- shrink a, is <- shrink i, e1s <- shrink e1, e2s <- shrink e2, ss <- shrink s]
  shrink (TryStmt a b cs mf) = emptyStmtShrink a ++
                               [TryStmt as bs css mfs | as <- shrink a, bs <- shrink b, css <- shrink cs, mfs <- shrink mf]
  shrink (ThrowStmt a e) = emptyStmtShrink a ++
                           [ThrowStmt as es | as <- shrink a, es <- shrink e]
  shrink (ReturnStmt a e) = emptyStmtShrink a ++
                            [ReturnStmt as es | as <- shrink a, es <- shrink e]
  shrink (WithStmt a o s) = emptyStmtShrink a ++
                            [WithStmt as os ss | as <- shrink a, os <- shrink o, ss <- shrink s]
  shrink (VarDeclStmt a vds) = emptyStmtShrink a ++
                               [VarDeclStmt as vdss | as <- shrink a, vdss <- shrink vds]
  shrink (FunctionStmt a n pars b) = emptyStmtShrink a ++
                                     [FunctionStmt as ns parss bs | as <- shrink a, ns <- shrink n, parss <- shrink pars, bs <- shrink b]
    
emptyStmtShrink a = [EmptyStmt a2 | a2 <- shrink a]    

type LabelSubst   = Map (Id ()) (Id ())
emptyConstantPool = Data.Map.empty

instance (Data a, Arbitrary a) => Arbitrary (JavaScript a) where
  arbitrary = do {s <- liftM2 Script arbitrary arbitrary;
                  if isProgramFixable s then fixLabels s
                  else arbitrary}
  shrink (Script a ss) = [Script na nss | na <- shrink a, nss <- shrink ss]
  
-- | Fixes labels so that labeled breaks and continues refer to
-- existing labeled statements, enclosing them; also, reduces the size
-- of the label set. Assumes that the program has a proper syntactic
-- structure, i.e. 'isProgramFixable' s = True.
fixLabels :: (Data a) => JavaScript a -> Gen (JavaScript a)
fixLabels s = 
  fixBreakContinueLabels s >>= removeDuplicateLabels
          
-- | choose n elements from a list randomly
rChooseElem :: [a] -> Int -> Gen [a]
rChooseElem xs n | n > 0 && (not $ null xs) = 
  if n >= length xs then return xs
  else (vectorOf n $ choose (0, n-1)) >>=
       (\subst -> return $ foldr (\n ys -> (xs!!n):ys) [] subst)
rChooseElem _  _ = return [] 

-- | A predicate that tells us whether a program has a fixable/correct
-- label-break/continue structure.  The predicate imposes syntactic
-- restrictions on the break, continue and labeled statements as in
-- the ECMA spec
isProgramFixable :: (Data a ) => JavaScript a -> Bool
isProgramFixable (Script _ stmts) = 
  Prelude.and $ 
  Prelude.map 
             (\stmt -> isBreakContinueFixable stmt False False False) 
             stmts

-- | Imposes relaxed restrictions on break and continue per ECMAScript
-- 5 spec (page 92): any continue without a label should be nested
-- within an iteration stmt, any continue with a label should be
-- nested in a labeled statement (not necessarily with the same
-- label); any break statement without a label should be nested in an
-- iteration or switch stmt, any break statement with a label should
-- be nested in a labeled statement (not necessarily with the same
-- label).
isBreakContinueFixable :: (Data a) => Statement a -> 
                                      Bool -> 
                                      Bool -> 
                                      Bool ->
                                      Bool
isBreakContinueFixable stmt inLabeled inIter inSwitch =
  case stmt of
    ContinueStmt _ Nothing -> inIter
    ContinueStmt _ (Just label) -> inLabeled
    BreakStmt    _ Nothing -> inIter || inSwitch
    BreakStmt    _ (Just label) -> inLabeled
    LabelledStmt _ label _ -> 
      continue stmt True inIter inSwitch
    _ -> if isIterationStmt stmt then
             continue stmt inLabeled True inSwitch
         else if isSwitchStmt stmt then
                  continue stmt inLabeled inIter True 
              else True
  --  _ -> continue stmt inLabeled inIter inSwitch
  where continue stmt inLabeled inIter inSwitch =
                   and $ map (\s -> isBreakContinueFixable s inLabeled inIter inSwitch) (children stmt)
                   
-- | Removes duplicate labels from nested labeled statements in order
-- to impose restrictions on labeled statements as per ECMAScript 5
-- spec (page 95): nested labeled statements cannot have duplicating
-- labels.
removeDuplicateLabels :: Data a => JavaScript a -> Gen (JavaScript a)
removeDuplicateLabels (Script x stmts) =
    return $ Script x (map (\stmt -> (evalState (transformM removeDL stmt) [])) stmts)
    where
      removeDL :: Statement a -> State [String] (Statement a)
      removeDL stmt@(LabelledStmt x lab s) = 
          do {enclosingLabels <- get;
              if Prelude.elem (unId lab) enclosingLabels then return s
              else modify ((:) $ unId lab) >> return stmt}
      removeDL s = return s        
      
-- | Selects a random element of the list
selectRandomElement :: [a] -> Gen a
selectRandomElement xs = 
  let l = length xs in
  do n <- arbitrary
     return $ xs !! (n `mod` l - 1)
-- | Changes labels of break/continue so that they refer to one of the
-- enclosing labels
fixBreakContinueLabels :: Data a => JavaScript a -> Gen (JavaScript a)
fixBreakContinueLabels (Script x stmts) =
  do stmts2 <- mapM (\stmt -> (evalStateT (fixBCL stmt) [])) stmts
     return $ Script x stmts2
    where
      fixBCL :: Data a => Statement a -> StateT [String] Gen (Statement a)
      fixBCL stmt@(LabelledStmt _ lab s) =
        do modify ((:) $ unId lab)
           descendM fixBCL stmt
      fixBCL stmt@(BreakStmt x (Just (Id y lab))) =
          do {labels <- get;
              if lab `notElem` labels then
                  do {newLab <- lift $ selectRandomElement labels;
                      return $ BreakStmt x (Just $ Id y newLab)}
              else return stmt}
      fixBCL stmt@(ContinueStmt x (Just (Id y lab))) =
          do {labels <- get;
              if lab `notElem` labels then
                  do {newLab <- lift $ selectRandomElement labels;
                      return $ ContinueStmt x (Just $ Id y newLab)}
              else return stmt}
      fixBCL s = return s

isSwitchStmt :: Statement a    -> Bool
isSwitchStmt (SwitchStmt _ _ _) = True
isSwitchStmt _                  = False