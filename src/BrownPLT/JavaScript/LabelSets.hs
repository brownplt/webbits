-- | Label-set analysis which annotates all the statements in the script
-- with their label sets according to ECMAScript specification,
-- section 12.12. The result of this analysis are useful for building
-- control-flow graphs.

module BrownPLT.JavaScript.LabelSets (annotateLabelSets 
                                     ,Label(..)) where

import BrownPLT.JavaScript.Syntax
import BrownPLT.JavaScript.Syntax.Annotations
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Generics.Uniplate.Data
import Data.Data (Data)

-- | Labels are either strings (identifiers) or "empty" (see 12.12 of
-- the spec)
data Label = Label String
           | EmptyLabel

-- | Annotates statements with their label sets; example use:
-- >>> let jsa = reannotate (\a -> (a, Set.empty))
-- >>> in  annotateLabelSets jsa (\labs (a, ls) -> (a, labs `Set.union` ls))
annotateLabelSets :: Data a =>
                     (Set Label -> a -> a) -- ^ annotation combination function
                  -> JavaScript a  -- ^ the script to annotate
                  -> JavaScript a
annotateLabelSets c = (transformBi $ annotateFuncStmtBodies c)
                    . (transformBi $ annotateFuncExprBodies c)
                    . (descendBi $ annotateStatements c)

annotateFuncStmtBodies :: Data a => (Set Label -> a -> a) 
                       -> Statement a
                       -> Statement a
annotateFuncStmtBodies f s = error ""
                       
                       
annotateFuncExprBodies :: Data a => (Set Label -> a -> a) 
                       -> Expression a 
                       -> Expression a
annotateFuncExprBodies f e = error ""

annotateStatements :: Data a => (Set Label -> a -> a)
                   -> Statement a   
                   -> Statement a
annotateStatements f s = error ""                   