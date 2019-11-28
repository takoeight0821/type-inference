{-# LANGUAGE TupleSections #-}
module Subst.Infer where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Subst.Subst
import           Subst.Type
import           Syntax

type Env = Map.Map String Scheme

type InferM a = ReaderT Env (State Int) a

runInfer :: InferM (Subst, Type) -> Scheme
runInfer m = flip evalState 0 $ (`runReaderT` mempty) $ do
  (s, t) <- m
  pure $ generalize mempty (apply s t)

unify :: Type -> Type -> Subst
unify (TyMeta a) t = bind a t
unify t (TyMeta a) = bind a t
unify (TyApp c1 ts1) (TyApp c2 ts2)
  | c1 == c2 = unifyMany ts1 ts2
unify _ _ = error "error(unify)"

unifyMany :: [Type] -> [Type] -> Subst
unifyMany [] [] = mempty
unifyMany (t1:ts1) (t2:ts2) =
  let s1 = unify t1 t2
      s2 = unifyMany (apply s1 ts1) (apply s1 ts2)
  in s2 `compose` s1
unifyMany _ _ = error "error(unifyMany)"

bind :: TyVar -> Type -> Subst
bind a t
  | t == TyMeta a = mempty
  | occursCheck a t = error "error(bind) : infinite type"
  | otherwise = Map.singleton a t

occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

newTyMeta :: InferM Type
newTyMeta = do
  s <- get
  modify (+1)
  return $ TyMeta s

instantiate :: Scheme -> InferM Type
instantiate (Forall as t) = do
  as' <- mapM (const newTyMeta) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: Env -> Type -> Scheme
generalize env t = Forall (Set.toList $ ftv t `Set.difference` ftv env) t

infer :: Exp -> InferM (Subst, Type)
infer (Var x) = do
  env <- ask
  case Map.lookup x env of
    Nothing -> error "error(lookupVar) : unbound variable"
    Just s  -> (mempty, ) <$> instantiate s
infer (Const Int{}) = return (mempty, TyApp IntC [])
infer (Const Bool{}) = return (mempty, TyApp BoolC [])
infer (App e1 e2) = do
  (s1, t1) <- infer e1
  (s2, t2) <- local (apply s1) $ infer e2
  retTy <- newTyMeta
  let s3 = unify (apply s2 t1) (TyApp ArrowC [t2, retTy])
  return (s3 `compose` s2 `compose` s1, apply s3 retTy)
infer (Lam x e) = do
  xTy <- newTyMeta
  (s, eTy) <- local (Map.insert x (Forall [] xTy)) $ infer e
  return (s, TyApp ArrowC [apply s xTy, eTy])
infer (Let x e1 e2) = do
  (s1, t1) <- infer e1
  scheme <- local (apply s1) $ asks generalize <*> pure t1
  (s2, t2) <- local (apply s1 . Map.insert x scheme) $ infer e2
  return (s2 `compose` s1, t2)
