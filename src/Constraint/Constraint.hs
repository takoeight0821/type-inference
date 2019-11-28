module Constraint.Constraint where

import           Constraint.Subst
import           Constraint.Type
import qualified Data.Map         as Map
import qualified Data.Set         as Set

data Constraint = Type :~ Type

instance Substitutable Constraint where
  apply s (t1 :~ t2) = apply s t1 :~ apply s t2
  ftv (t1 :~ t2) = ftv t1 `Set.union` ftv t2

runSolve :: [Constraint] -> Subst
runSolve cs = solver (mempty, cs)

solver :: (Subst, [Constraint]) -> Subst
solver (su, cs) = case cs of
  [] -> su
  ((t1 :~ t2) : cs0) ->
    let su1 = unify t1 t2
    in solver (su1 `compose` su, apply su1 cs0)

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
