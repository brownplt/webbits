module Main where

import Control.Monad (liftM )

import WebBits.Test
import WebBits.JavaScript.CoreTransform (simplify)

main = do
  str <- getContents
  let script = parse "stdin" str
  putStrLn $ pretty (simplify script)
