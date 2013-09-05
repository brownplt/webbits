-- | Experimental and very simple quasi-quotation of ECMAScript in
-- Haskell. Doesn't support anti-quotation as of now.

{-# LANGUAGE FlexibleContexts #-}
module Language.ECMAScript3.Syntax.QuasiQuote (js, jsexpr, jsstmt) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding (parse)
import Control.Monad.Identity
import Data.Data (Data)

import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Parser

jsexpr :: QuasiQuoter
jsexpr = QuasiQuoter {quoteExp = quoteJSExpr}

jsstmt :: QuasiQuoter
jsstmt = QuasiQuoter {quoteExp = quoteJSStmt}

js :: QuasiQuoter
js = QuasiQuoter {quoteExp = quoteJS}

quoteJSExpr :: String -> TH.ExpQ
quoteJSExpr = quoteCommon expression

quoteJSStmt :: String -> TH.ExpQ
quoteJSStmt = quoteCommon statement

quoteJS :: String -> TH.ExpQ
quoteJS = quoteCommon program

quoteCommon :: Data a => Parser String a -> String -> TH.ExpQ
quoteCommon p s = do loc <- TH.location
                     let fname = TH.loc_filename loc
                     let (line, col)  = TH.loc_start loc
                     let p2 = (getPosition >>= \pos ->
                                setPosition $ (flip setSourceName) fname $
                                (flip setSourceLine) line $
                                (flip setSourceColumn) col $ pos) >> p
                     case parse p2 "" s of
                       Left err -> do TH.report True $ show err
                                      return $ TH.UnboxedTupE []
                       Right x  -> dataToExpQ (const Nothing) x
