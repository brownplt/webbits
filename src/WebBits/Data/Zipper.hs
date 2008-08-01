module WebBits.Data.Zipper
  ( Tree(..)
  , Location
  
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

  , ZipperT
  , toLocation, fromLocation
  , nest                  -- Monad m => v -> ZipperT v m a -> ZipperT v m a
  , getNode               -- Monad m => ZipperT v m v
  , setNode               -- Monad m => v -> ZipperT v m ()
  , runZipperT
  , evalZipperT
  , execZipperT
  , shiftLeft, shiftRight   -- Monad m => ZipperT v m ()
  , shiftLeft', shiftRight' -- Monad m => ZipperT v m ()
  , withCurrentChild        -- Monad m => Zipper T v m a -> ZipperT v m a
  , maybeShiftRight
  
  , TraverserT, Traverser, runTraverserT, evalTraverserT, execTraverserT
  , trGet, trRight, trCanGoRight, trDown
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


--------------------------------------------------------------------------------
-- Tree combinators

dfsFold :: (w -> v -> w) -> w -> Tree v -> Tree w
dfsFold f w (Node v ts) = Node w' (map (dfsFold f w') ts)
  where w' = f w v

dfsFoldM :: Monad m => (w -> v -> m w) -> w -> Tree v -> m (Tree w)
dfsFoldM f w (Node v ts) = do
  w'  <- f w v
  ts' <- mapM (dfsFoldM f w') ts
  return $ Node w' ts'

showTree :: Show a => Tree a -> String
showTree tree =  drawTree (fmap show tree)
  

-------------------------------------------------------------------------------
-- Combinators!
--

empty :: a -> Tree a
empty a = Node a []

up :: Location a -> Location a
up (Location _ Top)               = error "Data/Zipper.hs: up of Top"
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

top :: Location a -> Location a
top loc@(Location _ Top) = loc
top loc@(Location _ _) = top (up loc)

--------------------------------------------------------------------------------
-- Zipper monad

type ZipperT v m a = StateT (Location v) m a

toLocation :: Tree a -> Location a
toLocation t = Location t Top

fromLocation :: Location a -> Tree a
fromLocation (Location t _) = t

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

nest :: Monad m => v -> ZipperT v m a -> ZipperT v m a
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

withCurrentChild :: Monad m => ZipperT v m a -> ZipperT v m a
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

maybeShiftRight :: Monad m => ZipperT v m ()
maybeShiftRight = do
  z <- get
  if canGoRight z
    then shiftRight
    else return ()
  
  
shiftLeft :: Monad m => ZipperT v m ()
shiftLeft = do
  z <- get
  put (left z)

shiftRight' :: Monad m => ZipperT v m ()
shiftRight' = do
  z <- get
  when (hasRight z)
       (put (right z))

       
--------------------------------------------------------------------------------
--

data Traverser a = Traverser a [Tree a] [Tree a] (Path a)

type TraverserT v m a = StateT (Traverser v) m a

toTraverser :: Tree a -> Path a -> Traverser a
toTraverser (Node v cs) path =  Traverser v [] cs path

runTraverserT :: Monad m => TraverserT v m a -> Tree v -> m (a, Tree v)
runTraverserT m (Node v ns) = do
  (a, Traverser v ls rs Top) <- runStateT m (Traverser v [] ns Top)
  return (a,Node v (reverse ls ++ rs))

evalTraverserT m t = runTraverserT m t >>= return . fst

execTraverserT m t = runTraverserT m t >>= return . snd

trGet :: Monad m => TraverserT v m v
trGet = do
  (Traverser v ls rs _) <- get
  return v
  

trRight :: Monad m => TraverserT v m ()
trRight = do
  (Traverser v ls (r:rs) path) <- get
  put (Traverser v (r:ls) rs path)
  
trCanGoRight :: Monad m => TraverserT v m Bool
trCanGoRight = do
  (Traverser _ _ rs _) <- get
  return $ not $ null rs


trDown :: Monad m => TraverserT v m a -> TraverserT v m a
trDown m = do
  (Traverser v ls (r:rs) path) <- get
  put (toTraverser r (Split v ls path rs))
  a <- m
  tr@(Traverser v' ls' rs' (Split v ls path rs)) <- get
  put $ Traverser v ls ((Node v' (reverse ls' ++ rs')):rs) path
  return a


shiftLeft' :: Monad m => ZipperT v m ()
shiftLeft' = do
  z <- get
  when (hasLeft z)
       (put (left z))
  
-------------------------------------------------------------------------------
-- Helper functions for shiftLeft' and shiftRight'

hasLeft :: Location a -> Bool
hasLeft (Location _ (Split _ (_:_) _ _)) = True
hasLeft _                                = False

hasRight :: Location a -> Bool
hasRight (Location _ (Split _ _ _ (_:_))) = True
hasRight _                                = False

