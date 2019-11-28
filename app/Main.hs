module Main where

import           Parse
import           Text.Megaparsec.Error
import qualified Ref.Infer as Ref
import qualified Subst.Infer as Subst
import qualified Constraint.Infer as Constraint

main :: IO ()
main = do
  src <- getContents
  case parseExp "<stdin>" src of
    Left  bundle -> putStr $ errorBundlePretty bundle
    Right ast    -> do
      print ast
      putStrLn "Ref:"
      t1 <- Ref.runInfer $ Ref.infer ast
      print t1
      putStrLn "Subst:"
      let t2 = Subst.runInfer $ Subst.infer ast
      print t2
      putStrLn "Constraint:"
      let t3 = Constraint.runInfer $ Constraint.infer ast
      print t3
