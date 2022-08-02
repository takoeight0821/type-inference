{-# OPTIONS_GHC -Wno-name-shadowing #-}
import qualified Constraint.Infer as Constraint
import Parse (parseExp)
import qualified Ref.Infer as Ref
import qualified Subst.Infer as Subst
import Test.Hspec
import Data.Either (isRight, isLeft)

main :: IO ()
main = hspec $ do
  checkAllTyping "fun x -> x"
  checkAllTyping "(fun x -> x) 1"
  checkAllInvalid "fun x -> x x"

checkAllTyping :: String -> SpecWith ()
checkAllTyping src = describe ("valid: " <> src) $ do
  let ast = parseExp "<test>" src
  it "Parse" $ ast `shouldSatisfy` isRight
  let Right ast' = ast
  typ <- runIO $ Ref.runInfer $ Ref.infer ast'
  it "Ref" $ typ `shouldSatisfy` isRight
  runIO $ print typ
  let typ = Subst.runInfer $ Subst.infer ast'
  it "Subst" $ typ `shouldSatisfy` isRight
  runIO $ print typ
  let typ = Constraint.runInfer $ Constraint.infer ast'
  it "Constraint" $ typ `shouldSatisfy` isRight
  runIO $ print typ

checkAllInvalid :: String -> SpecWith ()
checkAllInvalid src = describe ("invalid: " <> src) $ do
  let ast = parseExp "<test>" src
  it "Parse" $ ast `shouldSatisfy` isRight
  let Right ast' = ast
  typ <- runIO $ Ref.runInfer $ Ref.infer ast'
  it "Ref" $ typ `shouldSatisfy` isLeft
  let typ = Subst.runInfer $ Subst.infer ast'
  it "Subst" $ typ `shouldSatisfy` isLeft
  let typ = Constraint.runInfer $ Constraint.infer ast'
  it "Constraint" $ typ `shouldSatisfy` isLeft