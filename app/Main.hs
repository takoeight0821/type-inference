module Main where

import           Parse
import           Text.Megaparsec.Error
import           Ref.Infer

main :: IO ()
main = do
  src <- getContents
  case parseExp "<stdin>" src of
    Left  bundle -> putStr $ errorBundlePretty bundle
    Right ast    -> do
      print ast
      (t, env) <- runInfer $ infer ast
      print t
      print env
