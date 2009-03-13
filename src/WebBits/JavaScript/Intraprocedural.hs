-- |Generates an intraprocedural control flow graph for a single JavaScript
-- procedure.
module WebBits.JavaScript.Intraprocedural
  ( intraprocGraph
  , intraprocGraph'
  , Graph
  )  where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Control.Monad.State.Strict
import Text.ParserCombinators.Parsec.Pos (SourcePos, initialPos)
import Data.Generics
import WebBits.JavaScript.Core
import WebBits.Common() -- Data SourcePos
import System.IO.Unsafe

noPos = initialPos "Intraprocedural.hs"

type Node = (Int,Stmt (Int,SourcePos))
type Graph = Gr (Stmt (Int,SourcePos)) ()

numberStmts :: Stmt SourcePos -> Stmt (Int,SourcePos)
numberStmts stmts = evalState (everywhereM (mkM numberM) stmts') 0 where

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
type Stack = [(String,(Int,Int))]


stackReturn :: Stack -> Int
stackReturn [] = error "stackReturn : empty stack (should have return)"
stackReturn stack = fst $ snd $ L.last stack

initStack :: Stmt (Int,SourcePos) -> Stack
initStack exitStmt = 
  [("$exit not find",(fst $ stmtLabel exitStmt,error "next of ExitStmt"))]

nextStmt :: String -> Stack -> Int
nextStmt n stack = case lookup n stack of
  Just (_,s) -> s
  Nothing -> error "nextStmt: not on stack"

toStmt :: String -> Stack -> Int
toStmt n stack = case lookup n stack of
  Just (s,_) -> s
  Nothing -> error "toStmt: not on stack"


edge :: Int -> Int -> State Graph ()
edge i1 i2 = do
  gr <- get
  put (G.insEdge (i1,i2,()) gr)

edgeCreate :: Stmt (Int,SourcePos) 
           -> Int
           -> State Graph ()
edgeCreate stmtSrc dest = do
  src <- node stmtSrc
  edge src dest

node :: Stmt (Int,SourcePos) -> State Graph Int
node stmt = do
  gr <- get
  let ix = fst $ stmtLabel stmt
  put (G.insNode (ix,stmt) gr)
  return ix

stmt :: Stack
     -- ^The control stack is used to translate "structured gotos"
     -> Int
     -- ^The next statement.  We will usually add an edge to this statement.
     -> Stmt (Int,SourcePos)
     -> State Graph ()
stmt stack next s = case s of
  SeqStmt a [] -> fail "empty sequence"
  SeqStmt a ss -> do
    edgeCreate s (fst $ stmtLabel (head ss))
    -- s1 -> s2, s2 -> s3, ... , s_n -> next
    mapM2 (stmt stack) (tail (map (fst.stmtLabel) ss) ++ [next]) ss
    return ()
  BreakStmt a n -> edgeCreate s (nextStmt n stack)
  ContinueStmt _ n -> edgeCreate s (toStmt n stack)
  EmptyStmt _ -> edgeCreate s next
  AssignStmt _ _ _ -> edgeCreate s next
  DeleteStmt _ _ _ -> edgeCreate s next
  NewStmt _ _ _ _ -> edgeCreate s next -- TODO: node splitting?
  CallStmt _ _ _ _ -> edgeCreate s next
  MethodCallStmt _ _ _ _ _ -> edgeCreate s next
  IndirectMethodCallStmt _ _ _ _ _ -> edgeCreate s next
  IfStmt _ _ s1 s2 -> do
    i <- node s
    edge i (fst $ stmtLabel s1)
    edge i (fst $ stmtLabel s2)
    stmt stack next s1
    stmt stack next s2
  WhileStmt _ _ s1 -> do
    i <- node s
    -- The next statement after executing the body, is to renter the loop
    stmt stack i s1
    -- Eventually, the loop condition will be false and the next statement will
    -- be next.  The only way to directly jump to next out of the body, is to
    -- break.
    edge i next
  ForInStmt _ _ _ s1 -> do
    i <- node s
    stmt stack i s1
    edge i next
  TryStmt _ body _ catch finally -> do
    -- TODO: account for catch
    -- TODO: This treatment of finally is incorrect
    edgeCreate s (fst $ stmtLabel body)
    stmt stack (fst $ stmtLabel finally) body
    stmt stack next finally
  ThrowStmt _ _ -> return () -- TODO: um...
  ReturnStmt _  _ -> 
    edgeCreate s (stackReturn stack)
  LabelledStmt _ lbl s1 -> do
    i <- node s
    edge i (fst $ stmtLabel s1)
    stmt ((lbl,(i,next)):stack) next s1
  SwitchStmt _ _ cases -> do
    i <- node s
    mapM (edge i) (map (fst.stmtLabel.snd) cases)
    -- Each block may fallthrough to the next block
    mapM2 (stmt stack) 
          (map (fst.stmtLabel.snd) (tail cases) ++ [next])
          (map snd cases) 
    return ()
  EnterStmt _ -> edgeCreate s next
  ExitStmt _ -> return () -- next == s for convenience


stmtToNode :: Stmt (Int,SourcePos)
           -> (Int,Stmt (Int,SourcePos))
stmtToNode stmt = (fst $ stmtLabel stmt,stmt)

intraprocGraph' :: Stmt SourcePos -> Graph
intraprocGraph' stmt = snd (intraprocGraph noPos noPos stmt)

-- |Build an intraprocedural graph for the body of a procedure.  This function
--  will add the 'EnterStmt' and 'ExitStmt' nodes.  All the nodes will be
-- labelled with integers starting from zero.
intraprocGraph :: SourcePos -- ^location of the entry point
               -> SourcePos -- ^location of the exit point
               -> Stmt SourcePos -> (Stmt (Int,SourcePos),Graph)
intraprocGraph enterPos exitPos body = (full,graph) where

  -- Small trick to turn the EnterStmt and the body into a single numbered
  -- statement.  It's always safe to add/remove additional enclosing blocks.
  body' = SeqStmt noPos [EnterStmt enterPos, body, ExitStmt exitPos]
  full@(SeqStmt _ [labelledEnterStmt,labelledBody,labelledExitStmt]) = 
    numberStmts body'
  
  exitNode = stmtToNode labelledExitStmt
  enterNode = stmtToNode labelledEnterStmt

  initialGraph :: Gr (Stmt (Int,SourcePos)) ()
  initialGraph = G.insEdge (fst enterNode,fst $ stmtLabel labelledBody,()) $
                 G.insNode enterNode $ 
                 G.insNode exitNode G.empty

  graph = execState 
            (stmt (initStack labelledExitStmt)
                  (fst $ stmtLabel labelledExitStmt)
                  labelledBody)
            initialGraph

