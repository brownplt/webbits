{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}
module Language.ECMAScript5.ParserState 
       ( Comment(..)
       , SourceSpan(..) 
       , Positioned
       , ParserAnnotation
       , ParserState
       , InParserState
       , HasComments
       , Parser
       , PosParser
       , withPos
       , postfixWithPos
       , prefixWithPos
       , infixWithPos
       , getComments
       , allowIn
       , liftIn
       , withIn
       , withNoIn
       , assertInAllowed
       , changeState
       , initialParserState
       , modifyNewLine
       , modifyLabelSet
       , modifyComments
       , pushLabel
       , clearLabelSet
       , pushEnclosing
       , popEnclosing
       , withFreshEnclosing
       , setNewLineState 
       , hadNewLine 
       , hadNoNewLine
       , encIter
       , encOther
       , encSwitch
       , getEnclosing
       , isIter
       , isIterSwitch
       , HasLabelSet (..)
       ) where 
 
import Text.Parsec hiding (labels) 
import Text.Parsec.Pos (initialPos)
--import Test.Parsec.Prim
import Language.ECMAScript5.Syntax 
import Language.ECMAScript5.Syntax.Annotations 
import Data.Default.Class 
import Data.Default.Instances.Base 
import Control.Monad.Identity 
import Control.Applicative
import Control.Monad.State (modify)
 
type Positioned x = x ParserAnnotation 
 
type Parser   a = forall s. Stream s Identity Char => ParsecT s ParserState Identity a 
type InParser a =  forall s. Stream s Identity Char => ParsecT s InParserState Identity a 
type PosParser x = Parser (Positioned x) 
 
data ParserState = ParserState { hasNewLine :: Bool, comments :: [Comment], enclosing :: [EnclosingStatement], labelSet :: [Label] } 
data InParserState = InParserState { allowIn :: Bool, baseState :: ParserState } 

data EnclosingStatement = EnclosingIter [Label]
                          -- ^ The enclosing statement is an iteration statement
                        | EnclosingSwitch [Label]
                          -- ^ The enclosing statement is a switch statement
                        | EnclosingOther [Label]
                          -- ^ The enclosing statement is some other
                          -- statement.  Note, `EnclosingOther` is
                          -- never pushed if the current `labelSet` is
                          -- empty, so the list of labels in this
                          -- constructor should always be non-empty

isIter :: EnclosingStatement -> Bool
isIter (EnclosingIter _) = True
isIter _                 = False

isIterSwitch :: EnclosingStatement -> Bool
isIterSwitch (EnclosingIter _)   = True
isIterSwitch (EnclosingSwitch _) = True
isIterSwitch _                   = False

class HasLabelSet a where
  getLabelSet :: a -> [Label]
  setLabelSet :: [Label] -> a -> a

modifyLabelSet :: HasLabelSet a => ([Label] -> [Label]) -> a -> a
modifyLabelSet f a = setLabelSet (f $ getLabelSet a) a

instance HasLabelSet EnclosingStatement where
  getLabelSet e = case e of
    EnclosingIter ls   -> ls
    EnclosingSwitch ls -> ls
    EnclosingOther ls  -> ls
  setLabelSet ls e = case e of
    EnclosingIter _   -> EnclosingIter ls
    EnclosingSwitch _ -> EnclosingSwitch ls
    EnclosingOther _  -> EnclosingOther ls

instance HasLabelSet ParserState where
  getLabelSet ps = labelSet ps
  setLabelSet ls ps = ps {labelSet = ls}

type Label = String

data SourceSpan =  
  SourceSpan (SourcePos, SourcePos) 
 
data Comment  
  = SingleLineComment String  
  | MultiLineComment String  
    deriving Show 
 
class HasComments a where 
  getComments :: a -> [Comment] 
  setComments :: a -> [Comment] -> a 
  modifyComments :: ([Comment] -> [Comment]) -> a -> a 
  modifyComments f st = setComments st (f $ getComments st) 
 
instance HasComments ParserState where 
  getComments = comments 
  setComments st cs = st { comments = cs } 
 
instance HasComments InParserState where 
  getComments = comments . baseState 
  setComments st cs = st { baseState = setComments (baseState st) cs } 
 
type ParserAnnotation = (SourceSpan, [Comment]) 
 
instance Default SourcePos where 
  def = initialPos "" 
 
instance Default SourceSpan where 
  def = SourceSpan def 
 
instance Show SourceSpan where 
  show (SourceSpan (p1,p2)) = let 
    l1 = show $ sourceLine p1 - 1 
    c1 = show $ sourceColumn p1 - 1 
    l2 = show $ sourceLine p2 - 1 
    c2 = show $ sourceColumn p2 - 1 
    s1 = l1 ++ "-" ++ c1 
    s2 = l2 ++ "-" ++ c2 
    in "(" ++ show (s1 ++ "/" ++ s2) ++ ")" 
 
consumeComments :: (HasComments state) => Stream s Identity Char => ParsecT s state Identity [Comment] 
consumeComments = do comments <- getComments <$> getState 
                     modifyState $ modifyComments (const []) 
                     return comments 
 
-- a convenience wrapper to take care of the position, "with
-- position". Whenever we return something `Positioned` we need to use
-- it.
withPos   :: (HasAnnotation x, HasComments state, Stream s Identity Char) 
          => ParsecT s state Identity (Positioned x) 
          -> ParsecT s state Identity (Positioned x) 
withPos p = do start <- getPosition 
               comments <- consumeComments 
               result <- p 
               end <- getPosition 
               return $ setAnnotation (SourceSpan (start, end), comments) result 
 
postfixWithPos :: (HasAnnotation x, HasComments state, Stream s Identity Char) => 
                  ParsecT s state Identity (Positioned x -> Positioned x) ->  
                  ParsecT s state Identity (Positioned x -> Positioned x) 
postfixWithPos p = do 
  f <- p 
  high <- getPosition 
  comments <- consumeComments 
  return $ \e -> let (SourceSpan (low, _), _) = getAnnotation e  
                 in setAnnotation (SourceSpan (low, high), comments) (f e) 
 
prefixWithPos :: (HasAnnotation x, HasComments state, Stream s Identity Char) => 
                  ParsecT s state Identity (Positioned x -> Positioned x) ->  
                  ParsecT s state Identity (Positioned x -> Positioned x) 
prefixWithPos p = do 
  low <- getPosition 
  f <- p 
  comments <- consumeComments 
  return $ \e -> let (SourceSpan (_, high), _) = getAnnotation e  
                 in setAnnotation (SourceSpan (low, high), comments) (f e) 

infixWithPos :: (HasAnnotation x, HasComments state, Stream s Identity Char) =>
                ParsecT s state Identity (Positioned x -> Positioned x -> Positioned x) ->
                ParsecT s state Identity (Positioned x -> Positioned x -> Positioned x)
infixWithPos p = 
  liftAnnotations2 combinePos <$> p
  where combinePos (SourceSpan (low, _), _) (SourceSpan (_, high), _) = (SourceSpan (low, high), [])
        liftAnnotations2 f g x y = setAnnotation (f (getAnnotation x) (getAnnotation y)) (g x y)



liftIn :: Bool -> Parser a -> InParser a 
liftIn x p = changeState (InParserState x) baseState p 
 
withIn, withNoIn :: InParser a -> Parser a 
withIn   p = changeState baseState (InParserState True) p 
withNoIn p = changeState baseState (InParserState False) p 
 
assertInAllowed :: InParser () 
assertInAllowed = getState >>= guard.allowIn 
 
changeState 
  :: forall m s u v a . (Functor m, Monad m) 
  => (u -> v) 
  -> (v -> u) 
  -> ParsecT s u m a 
  -> ParsecT s v m a 
changeState forward backward = mkPT . transform . runParsecT 
  where 
    mapState f st = st { stateUser = f (stateUser st) } 
    mapReply f (Ok a st err) = Ok a (mapState f st) err 
    mapReply _ (Error e) = Error e 
    transform p st = (fmap . fmap . fmap) (mapReply forward) (p (mapState backward st)) 
 
modifyNewLine  f st = st { hasNewLine = f (hasNewLine st) }
modifyEnclosing f st = st { enclosing = f (enclosing st) }
 
initialParserState :: ParserState 
initialParserState = ParserState False [] [] []

-- | checks if the label is not yet on the stack, if it is -- throws 
-- an error; otherwise it pushes it onto the stack 
pushLabel :: Id a -> Parser (Id a)
pushLabel ident = do ps <- getState 
                     pos <- getPosition
                     let lab  = unId ident
                     let labs = labelSet ps
                     if lab `elem` labs 
                       then fail $ "Duplicate label at " ++ show pos 
                       else putState (ps {labelSet = (lab:labs)}) >> return ident

clearLabelSet :: Parser ()
clearLabelSet = modifyState $ modifyLabelSet (const [])

encOther = EnclosingOther []
encSwitch = EnclosingSwitch []
encIter = EnclosingIter []

applyLabelSet :: EnclosingStatement -> Parser ()
applyLabelSet enc = do ls <- labelSet <$> getState
                       clearLabelSet
                       case (enc, ls) of
                         (EnclosingOther _, []) -> return ()
                         _ -> modifyState $ modifyEnclosing ((setLabelSet ls enc):)

pushEnclosing :: EnclosingStatement -> Parser ()
pushEnclosing = applyLabelSet
 
popEnclosing :: Parser () 
popEnclosing = modifyState (modifyEnclosing safeTail) 
  where safeTail [] = [] 
        safeTail (_:xs) = xs 

clearEnclosing = modifyEnclosing (const [])

getEnclosing :: Parser [EnclosingStatement]
getEnclosing = enclosing <$> getState

withFreshEnclosing :: Parser a -> Parser a 
withFreshEnclosing p = do oldState <- getState 
                          putState $ clearEnclosing oldState 
                          a <- p 
                          putState oldState 
                          return a 
 
-- was newline consumed? keep as parser state set in 'ws' parser 
 
setNewLineState :: [Bool] -> Parser Bool 
setNewLineState wsConsumed = 
  let consumedNewLine = any id wsConsumed in do 
    modifyState $ modifyNewLine (const consumedNewLine) 
    return consumedNewLine 
 
hadNewLine :: Parser () 
hadNewLine = hasNewLine <$> getState >>= guard 
 
hadNoNewLine :: Parser() 
hadNoNewLine = hasNewLine <$> getState >>= guard.not
