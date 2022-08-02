{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module Constraint.Infer where

import Constraint.Constraint (Constraint (..), runSolve)
import Constraint.Type
  ( Scheme (..),
    TyCon (ArrowC, BoolC, IntC),
    Type (..),
  )
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.Reader
  ( MonadReader (ask, local),
    ReaderT (runReaderT),
  )
import Control.Monad.State
  ( MonadState (get),
    StateT,
    evalStateT,
    modify,
  )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Subst.Subst (Substitutable (apply, ftv))
import Syntax (Const (Bool, Int), Exp (..))

type Env = Map.Map String Scheme

type Error = String

type InferM a = ReaderT Env (StateT Int (Except Error)) a

runInfer :: InferM ([Constraint], Type) -> Either Error Scheme
runInfer m =
  runExcept $
    flip evalStateT (0 :: Int) $
      flip runReaderT (mempty :: Env) $ do
        (cs, t) <- m
        s <- runSolve cs
        pure $ generalize mempty (apply s t)

newTyMeta :: InferM Type
newTyMeta = do
  s <- get
  modify (+ 1)
  return $ TyMeta s

instantiate :: Scheme -> InferM Type
instantiate (Forall as t) = do
  as' <- mapM (const newTyMeta) as
  let su = Map.fromList (zip as as')
  return (apply su t)

generalize :: Env -> Type -> Scheme
generalize env t = Forall (Set.toList $ ftv t `Set.difference` ftv env) t

infer :: Exp -> InferM ([Constraint], Type)
infer (Var x) = do
  env <- ask
  case Map.lookup x env of
    Nothing -> throwError "error(lookupVar) : unbound variable"
    Just s -> ([],) <$> instantiate s
infer (Const Int {}) = return ([], TyApp IntC [])
infer (Const Bool {}) = return ([], TyApp BoolC [])
infer (App e1 e2) = do
  (cs1, t1) <- infer e1
  (cs2, t2) <- infer e2
  retTy <- newTyMeta
  return ((TyApp ArrowC [t2, retTy] :~ t1) : cs2 <> cs1, retTy)
infer (Lam x e) = do
  xTy <- newTyMeta
  (cs, eTy) <- local (Map.insert x (Forall [] xTy)) $ infer e
  return (cs, TyApp ArrowC [xTy, eTy])
infer (Let x e1 e2) = do
  (cs1, t1) <- infer e1
  sub <- runSolve cs1
  env <- ask
  let scheme = generalize (apply sub env) (apply sub t1)
  (cs2, t2) <- local (apply sub . Map.insert x scheme) $ infer e2
  return (cs1 <> cs2, t2)
