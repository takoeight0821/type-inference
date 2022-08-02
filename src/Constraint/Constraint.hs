{-# LANGUAGE FlexibleContexts #-}

module Constraint.Constraint where

import Constraint.Subst
import Constraint.Type
import Control.Monad.Except (MonadError (throwError))
import qualified Data.Map as Map
import qualified Data.Set as Set

type Error = String

data Constraint = Type :~ Type
  deriving (Eq, Show)

instance Substitutable Constraint where
  apply s (t1 :~ t2) = apply s t1 :~ apply s t2
  ftv (t1 :~ t2) = ftv t1 `Set.union` ftv t2

runSolve :: MonadError Error m => [Constraint] -> m Subst
runSolve cs = solver (mempty, cs)

solver :: MonadError Error m => (Subst, [Constraint]) -> m Subst
solver (su, cs) = case cs of
  [] -> pure su
  ((t1 :~ t2) : cs0) -> do
    su1 <- unify t1 t2
    solver (su1 `compose` su, apply su1 cs0)

unify :: MonadError Error m => Type -> Type -> m Subst
unify (TyMeta a) t = bind a t
unify t (TyMeta a) = bind a t
unify (TyApp c1 ts1) (TyApp c2 ts2)
  | c1 == c2 = unifyMany ts1 ts2
unify _ _ = throwError "error(unify)"

unifyMany :: MonadError Error m => [Type] -> [Type] -> m Subst
unifyMany [] [] = pure mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
  pure $ s2 `compose` s1
unifyMany _ _ = throwError "error(unifyMany)"

bind :: MonadError Error m => TyVar -> Type -> m Subst
bind a t
  | t == TyMeta a = pure mempty
  | occursCheck a t = throwError $ "error(bind) : infinite type " <> show a <> " " <> show t
  | otherwise = pure $ Map.singleton a t

occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
