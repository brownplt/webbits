-- |Generates an intraprocedural control flow graph for a single JavaScript
-- procedure.
module WebBits.JavaScript.Intraprocedural
  ( intraprocGraph
  , Edges
  )  where

import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad.State.Strict
import Text.ParserCombinators.Parsec.Pos (SourcePos, initialPos)
import Data.Generics
import WebBits.JavaScript.Core
import WebBits.Common() -- Data SourcePos

noPos = initialPos "Intraprocedural.hs"

numStmts :: Stmt (Int,SourcePos) -> Int
numStmts s = gcount (mkQ False isStmt) s where
  isStmt :: Stmt (Int,SourcePos) -> Bool
  isStmt _ = True

numberStmts :: Stmt SourcePos -> Stmt (Int,SourcePos)
numberStmts stmts = evalState (gmapM (mkM numberM) stmts') 0 where
  stmts' :: Stmt (Int,SourcePos)
  stmts' = fmap (\p -> (0,p)) stmts
  numberM :: (Int,SourcePos) -> State Int (Int,SourcePos)
  numberM (_,p) = do
    n <- get
    put (n+1)
    return (n,p)


mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 f [] _ = return []
mapM2 f _ [] = return []
mapM2 f (x:xs) (y:ys) = do
  z <- f x y
  zs <- mapM2 f xs ys
  return (z:zs)

-- ^The control stack of statements, along with the next statement
-- for each statement in this stack.
type Stack = [(String,(Stmt (Int,SourcePos),Stmt (Int,SourcePos)))]

type Edges = M.Map Int [Int]

stackReturn :: Stack -> Stmt (Int,SourcePos)
stackReturn [] = error "stackReturn : empty stack (should have return)"
stackReturn stack = fst $ snd $ L.last stack

initStack :: Stmt (Int,SourcePos) -> Stack
initStack exitStmt = [("$exit not find",(exitStmt,error "next of ExitStmt"))]

nextStmt :: String -> Stack -> Stmt (Int,SourcePos)
nextStmt n stack = case lookup n stack of
  Just (_,s) -> s
  Nothing -> error "nextStmt: not on stack"

toStmt :: String -> Stack -> Stmt (Int,SourcePos)
toStmt n stack = case lookup n stack of
  Just (s,_) -> s
  Nothing -> error "toStmt: not on stack"

edge :: Stmt (Int,SourcePos) -> Stmt (Int,SourcePos) -> State Edges ()
edge s1 s2 = do
  m <- get
  let l1 = fst $ stmtLabel s1
  let l2 = fst $ stmtLabel s2
  put (M.insertWith' (const $ (l2:)) l1 [l2] m)


stmt :: Stack
     -- ^The control stack is used to translate "structured gotos"
     -> Stmt (Int,SourcePos)
     -- ^The next statement.  We will usually add an edge to this statement.
     -> Stmt (Int,SourcePos)
     -> State Edges ()
stmt stack next s = case s of
  SeqStmt a [] -> fail "empty sequence"
  SeqStmt a ss -> do
    edge s (head ss)
    mapM2 (stmt stack) ss (tail ss ++ [next])
    return ()
  BreakStmt a n ->
    edge s (nextStmt n stack)
  ContinueStmt _ n -> edge s (toStmt n stack)
  EmptyStmt _ -> edge s next
  AssignStmt _ _ _ -> edge s next
  DeleteStmt _ _ _ -> edge s next
  NewStmt _ _ _ _ -> edge s next -- TODO: node splitting?
  CallStmt _ _ _ _ -> edge s next
  MethodCallStmt _ _ _ _ _ -> edge s next
  IndirectMethodCallStmt _ _ _ _ _ -> edge s next
  IfStmt _ _ s1 s2 -> do
    edge s s1
    edge s s2
    stmt stack next s1
    stmt stack next s2
  WhileStmt _ _ s1 -> do
    -- The next statement after executing the body, is to renter the loop
    stmt stack s s1
    -- Eventually, the loop condition will be false and the next statement will
    -- be next.  The only way to directly jump to next out of the body, is to
    -- break.
    edge s next
  ForInStmt _ _ _ s1 -> do
    stmt stack s s1
    edge s next
  TryStmt _ body _ catch finally -> do
    -- TODO: account for catch
    -- TODO: This treatment of finally is incorrect
    stmt stack finally body
    stmt stack next finally
  ThrowStmt _ _ -> return () -- TODO: um...
  ReturnStmt _  _ -> 
    edge s (stackReturn stack)
  LabelledStmt _ lbl s1 -> do
    edge s s1
    stmt ((lbl,(s,next)):stack) next s1
  SwitchStmt _ _ cases -> do
    mapM (edge s) (map snd cases)
    mapM2 (stmt stack) (map snd cases) 
          (map snd (tail cases) ++ [next])
    return ()
  ExitStmt _ -> return () -- next == s for convenience

-- |Build an intraprocedural graph for the body of a procedure.  This function
--  will add the 'EnterStmt' and 'ExitStmt' nodes.  All the nodes will be
-- labelled with integers starting from zero.
intraprocGraph :: SourcePos -- ^location of the entry point
               -> SourcePos -- ^location of the exit point
               -> Stmt SourcePos -> (Stmt (Int,SourcePos), Edges)
intraprocGraph enterPos exitPos body = (full,graph) where

  -- Small trick to turn the EnterStmt and the body into a single numbered
  -- statement.  It's always safe to add/remove additional enclosing blocks.
  body' = SeqStmt noPos [SeqStmt noPos [EnterStmt enterPos, body], 
                         ExitStmt exitPos]
  full@(SeqStmt _ [labelledBody,labelledExitStmt]) =  numberStmts body'

  graph = execState 
            (stmt (initStack labelledExitStmt)
                  labelledExitStmt
                  labelledBody)
            M.empty
  
