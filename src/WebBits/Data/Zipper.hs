-- |A zipper for "Data.Tree".
module WebBits.Data.Zipper
  ( 
  -- * Functional zipper
  
  -- $functionalZipper
  
    Tree(..)
  , Location
  , toLocation, fromLocation  
  , dfsFold 
  , dfsFoldM
  , showTree

  , empty
  , up, down, left, right -- Location a -> Location a
  , replace
  , change
  , insertDown, insertLeft, insertRight
  , isTop, isChild
  , getValue
  , subTree
  , top
  , canGoLeft, canGoRight, canGoUp, canGoDown -- Location a -> Bool
  
  -- * Imperative tree construction
  
  -- $treeBuilder
  
  , ZipperT

  , nest                  -- Monad m => v -> ZipperT v m a -> ZipperT v m a
  , getNode               -- Monad m => ZipperT v m v
  , setNode               -- Monad m => v -> ZipperT v m ()
  , runZipperT
  , evalZipperT
  , execZipperT
  , shiftLeft, shiftRight   -- Monad m => ZipperT v m ()
  , shiftLeft', shiftRight' -- Monad m => ZipperT v m ()
  , withCurrentChild        -- Monad m => Zipper T v m a -> ZipperT v m a
  
  ) where

import Control.Monad
import Control.Monad.State
import Data.Tree (Tree (..), drawTree)

-------------------------------------------------------------------------------
-- Data types!
--

data Path a
  = Top
  | Split a [Tree a] (Path a) [Tree a]

data Location a = Location (Tree a) (Path a)


dfsFold :: (w -> v -> w) -- ^transforms a node 'v' using the accumulated
                         -- value 'w' from the root of the tree to 'v'
        -> w             -- ^the initial value of the accumulator at the root
        -> Tree v 
        -> Tree w
dfsFold f w (Node v ts) = Node w' (map (dfsFold f w') ts)
  where w' = f w v

-- |Similar to 'dfsFold', but the transformation is done in the monad 'm'.  The
-- sequence of operations is depth-first, left-to-right.
dfsFoldM :: Monad m => (w -> v -> m w) -> w -> Tree v -> m (Tree w)
dfsFoldM f w (Node v ts) = do
  w'  <- f w v
  ts' <- mapM (dfsFoldM f w') ts
  return $ Node w' ts'

-- Uses 'Data.Tree.drawShow'.
showTree :: Show a => Tree a -> String
showTree tree =  drawTree (fmap show tree)
  

-------------------------------------------------------------------------------
-- Combinators!
--

-- $functionalZipper
-- These are the core zipper functions.

empty :: a -> Tree a
empty a = Node a []

up :: Location a -> Location a
up (Location _ Top)               = error " up of Top"
up (Location t (Split v ls p rs)) = Location (Node v (reverse ls ++ (t:rs))) p

down :: Location a -> Location a
down (Location (Node v []) _)     = error "down of empty"
down (Location (Node v (t:ts)) p) = Location t (Split v [] p ts)

left :: Location a -> Location a
left (Location t Top)                   = error "left of top"
left (Location t (Split v [] p rs))     = error "left on leftmost"
left (Location t (Split v (l:ls) p rs)) = Location l (Split v ls p (t:rs))

right :: Location a -> Location a
right (Location t Top) = 
  error "Data.Zipper.right : at the top"
right (Location t (Split v ls p []))     = error "right on rightmost"
right (Location t (Split v ls p (r:rs))) = Location r (Split v (t:ls) p rs)

replace :: Location a -> Tree a ->  Location a
replace (Location _ p) t = Location t p

change :: Location a -> a -> Location a
change (Location (Node _ cs) p) t = Location (Node t cs) p

insertDown :: Location a -> Tree a -> Location a
insertDown (Location (Node v ts) p) t = Location (Node v (t:ts)) p

insertDownRight :: Location a -> Tree a -> Location a
insertDownRight (Location (Node v ts) p) t = 
  Location t (Split v (reverse ts) p [])

insertLeft :: Location a -> Tree a -> Location a
insertLeft (Location _ Top) _                = error "insert on top"
insertLeft (Location t (Split v ls p rs)) t' =
  Location t (Split v (t':ls) p rs)

insertRight :: Location a -> Tree a -> Location a
insertRight (Location _ Top) _                = error "insert on top"
insertRight (Location t (Split v ls p rs)) t' = 
  Location t (Split v ls p (t':rs))

isTop :: Location a -> Bool
isTop (Location _ Top) = True
isTop _                = False

isChild :: Location a -> Bool
isChild = not . isTop

canGoRight :: Location a -> Bool
canGoRight (Location _ (Split _ _ _ [])) = False
canGoRight (Location _ Top) = False
canGoRight _ = True

canGoDown :: Location a -> Bool
canGoDown (Location (Node _ []) _) = False
canGoDown _                        = True

canGoLeft :: Location a -> Bool
canGoLeft (Location _ Top)              = False
canGoLeft (Location _ (Split _ [] _ _)) = False
canGoLeft _                             = True

canGoUp :: Location a -> Bool
canGoUp (Location _ Top) = False
canGoUp _                = True

getValue :: Location a -> a
getValue (Location (Node v _) _) = v

subTree :: Location a -> Tree a
subTree (Location node _) = node

-- |Traverses to the top of the tree.
--
-- @
-- up.top = undefined
-- top.top = top
-- @
top :: Location a -> Location a
top loc@(Location _ Top) = loc
top loc@(Location _ _) = top (up loc)

toLocation :: Tree a -> Location a
toLocation t = Location t Top

fromLocation :: Location a -> Tree a
fromLocation (Location t _) = t


-- $treeBuilder
-- A state monad that carries a zipper.  It provides convenient methods to
-- imperatively create and update a tree.
--
-- The state monad's set method may be used to arbitrarily update the current
-- location.  However, such updates can break the behavior of nest and
-- withCurrentChild.  We recommend avoiding StateT.set.

type ZipperT v m a = StateT (Location v) m a


runZipperT :: Monad m => ZipperT v m a -> Location v -> m (a, Tree v)
runZipperT m l = do
  (a, Location t Top) <- runStateT m l
  return (a,t)

evalZipperT :: Monad m => ZipperT v m a -> Location v -> m a
evalZipperT m l = do
  ~(a, _) <- runStateT m l
  return a

execZipperT :: Monad m => ZipperT v m a -> Location v -> m (Tree v)
execZipperT m l = do
  ~(_, Location t Top) <- runStateT m l
  return t

-- |Creates a new node as the right-most child of the current node.
nest :: Monad m 
     => v -- ^value of the new right-most child 
     -> ZipperT v m a -- ^computation applied to the new child 
     -> ZipperT v m a -- ^returns the result of the nested computation
nest v m = do
  z <- get 
  put $ insertDownRight z (empty v)
  a <- m
  z' <- get -- z' is the child of z 
  put (up z')
  return a
  
getNode :: Monad m => ZipperT v m v
getNode = do
  (Location (Node v _) _)  <- get
  return v
  
setNode :: Monad m => v -> ZipperT v m ()
setNode v = do
  (Location (Node _ cs) path) <- get
  put $ Location (Node v cs) path

withCurrentChild :: Monad m 
                 => ZipperT v m a -- ^computation to apply to the current child 
                 -> ZipperT v m a -- ^returns the result of the nested 
                                  -- computation
withCurrentChild m = do
  z <- get
  put (down z)
  a <- m
  z' <- get
  put (up z')
  return a

shiftRight :: Monad m => ZipperT v m ()
shiftRight = do
  z <- get
  put (right z)

  
shiftLeft :: Monad m => ZipperT v m ()
shiftLeft = do
  z <- get
  put (left z)

-- |Silently fails to shift right if there is no right-child.
shiftRight' :: Monad m => ZipperT v m ()
shiftRight' = do
  z <- get
  when (hasRight z)
       (put (right z))

-- |Silently fails to shift left if there is no left-child.
shiftLeft' :: Monad m => ZipperT v m ()
shiftLeft' = do
  z <- get
  when (hasLeft z)
       (put (left z))

hasLeft :: Location a -> Bool
hasLeft (Location _ (Split _ (_:_) _ _)) = True
hasLeft _                                = False

hasRight :: Location a -> Bool
hasRight (Location _ (Split _ _ _ (_:_))) = True
hasRight _                                = False


