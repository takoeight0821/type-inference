module Main where

import           Parse
import           Text.Megaparsec.Error
import qualified Ref.Infer as Ref
import qualified Subst.Infer as Subst

main :: IO ()
main = do
  src <- getContents
  case parseExp "<stdin>" src of
    Left  bundle -> putStr $ errorBundlePretty bundle
    Right ast    -> do
      print ast
      putStr "Ref: "
      t1 <- Ref.runInfer $ Ref.infer ast
      print t1
      putStr "Subst: "
      let t2 = Subst.runInfer $ Subst.infer ast
      print t2
