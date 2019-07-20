module Main where

import           Parse
import           Text.Megaparsec.Error

main :: IO ()
main = do
  src <- getContents
  case parseExp "<stdin>" src of
    Left  bundle -> putStr $ errorBundlePretty bundle
    Right ast    -> print ast
