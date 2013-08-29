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
       , getComments
       , allowIn
       , liftIn
       , withIn
       , withNoIn
       , assertInAllowed
       , changeState
       , initialParserState
       , modifyNewLine
       , modifyLabels
       , modifyComments
       , pushLabel
       , popLabel
       , setNewLineState
       , hadNewLine
       , hadNoNewLine
       ) where

import Text.Parsec hiding (labels)
import Text.Parsec.Pos (initialPos)
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Data.Default.Class
import Data.Default.Instances.Base
import Control.Monad.Identity
import Control.Applicative

type Positioned x = x ParserAnnotation

type Parser   a = forall s. Stream s Identity Char => ParsecT s ParserState Identity a
type InParser a =  forall s. Stream s Identity Char => ParsecT s InParserState Identity a
type PosParser x = Parser (Positioned x)

data ParserState = ParserState { hasNewLine :: Bool, comments :: [Comment], labels :: [String] }
data InParserState = InParserState { allowIn :: Bool, baseState :: ParserState }

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
  def = undefined ""

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

-- a convenience wrapper to take care of the position, "with position"

withPos   :: (HasAnnotation x, HasComments state, Stream s Identity Char) => ParsecT s state Identity (Positioned x) -> ParsecT s state Identity (Positioned x)
withPos p = do start <- getPosition
               comments <- consumeComments
               result <- p
               end <- getPosition
               return $ setAnnotation (SourceSpan (start, end), comments) result

postfixWithPos :: HasAnnotation x =>
                  Parser (Positioned x -> Positioned x) -> 
                  Parser (Positioned x -> Positioned x)
postfixWithPos p = do
  f <- p
  high <- getPosition
  comments <- consumeComments
  return $ \e -> let (SourceSpan (low, _), _) = getAnnotation e 
                 in setAnnotation (SourceSpan (low, high), comments) (f e)

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

modifyLabels   f st = st { labels = f (labels st) }
modifyNewLine  f st = st { hasNewLine = f (hasNewLine st) }

initialParserState :: ParserState
initialParserState = ParserState False [] []

-- | checks if the label is not yet on the stack, if it is -- throws
-- an error; otherwise it pushes it onto the stack
pushLabel :: String -> Parser ()
pushLabel lab = do ParserState nl cs labs <- getState
                   pos <- getPosition
                   if lab `elem` labs
                     then fail $ "Duplicate label at " ++ show pos
                     else putState (ParserState nl cs (lab:labs))

popLabel :: Parser ()
popLabel = modifyState (modifyLabels safeTail)
  where safeTail [] = []
        safeTail (_:xs) = xs

clearLabels :: ParserState -> ParserState
clearLabels = modifyLabels (const [])

withFreshLabelStack :: Parser a -> Parser a
withFreshLabelStack p = do oldState <- getState
                           putState $ clearLabels oldState
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
