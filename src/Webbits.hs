module Main where

import Control.Monad (liftM )

import WebBits.Test
import WebBits.JavaScript.Simplify (simplify)
import WebBits.JavaScript.ToCore (jsToCore)
import WebBits.JavaScript.Env

main = do
  str <- getContents
  let script = parse "" str
  putStrLn (show (localVars script))
  let simplified = simplify script
  putStrLn $ pretty simplified
  putStrLn $ show (jsToCore simplified)
