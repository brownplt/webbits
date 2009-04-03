module BrownPLT.Html.Instances() where

import qualified Prelude as Prelude
import Prelude (Functor,map,fmap)
import qualified Data.List as List
import qualified Data.Char as Char
import Data.Foldable
import Data.Traversable
import Control.Applicative

import BrownPLT.Html.Syntax

ltraverse:: (Traversable t, Applicative f) => (a -> f b) -> [t a] -> f [t b]
ltraverse f [] = pure []
ltraverse f (x:xs) = pure (:) <*> (traverse f x) <*> ltraverse f xs

instance Functor (Attribute a) where
  fmap f (Attribute id val a)            = Attribute id val a
  fmap f (AttributeExpr a id script def) = AttributeExpr a id (f script) def

instance Functor (Html a) where
  fmap f (Element id attrs children a) = 
    Element id (map (fmap f) attrs) (map (fmap f) children) a
  fmap f (Text str a)                  = Text str a
  fmap f (Comment str a)               = Comment str a
  fmap f (HtmlSeq xs)                  = HtmlSeq (map (fmap f) xs)
  fmap f (ProcessingInstruction str a) = ProcessingInstruction str a
  fmap f (InlineScript script a def)   = InlineScript (f script) a def
  fmap f (Script script a)             = Script (f script) a

instance Foldable (Attribute a) where
  foldr f x (Attribute id val a)          = x
  foldr f x (AttributeExpr a id script def) = f script x

instance Foldable (Html a) where
  foldr f x (Element id attrs children a) =
    Prelude.foldr  (\child x' -> foldr f x' child) x children  
  foldr f x (Text str a)                  = x
  foldr f x (Comment str a)               = x
  foldr f x (HtmlSeq ys)                  = 
    Prelude.foldr (\child x' -> foldr f x' child) x ys
  foldr f x (ProcessingInstruction str a) = x
  foldr f x (InlineScript script a def)   = f script x
  foldr f x (Script script a)             = f script x

instance Traversable (Attribute a) where
  traverse f (Attribute id val a)            =
    pure (Attribute id val a)
  traverse f (AttributeExpr a id script def) =
    (AttributeExpr a id) <$> (f script) <*> pure def

instance Traversable (Html a) where
  traverse f (Element id attrs children a) = 
    (Element id) <$> ltraverse f attrs  <*> ltraverse f children <*> pure a
  traverse f (Text str a)                  = 
    pure (Text str a)
  traverse f (Comment str a)               = 
    pure (Comment str a)
  traverse f (HtmlSeq xs)                  = 
    HtmlSeq <$> ltraverse f xs
  traverse f (ProcessingInstruction str a) = 
    pure (ProcessingInstruction str a)
  traverse f (InlineScript script a def)   =
    InlineScript <$> (f script) <*> pure a <*> pure def
  traverse f (Script script a)             = 
    Script <$> (f script) <*> pure a
