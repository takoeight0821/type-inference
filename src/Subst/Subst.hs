module Subst.Subst where

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Subst.Type

type Subst = Map.Map TyVar Type

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = fmap (apply s1) s2 `Map.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TyVar

instance Substitutable Type where
  apply s t@(TyMeta a) = Map.findWithDefault t a s
  apply s (TyApp c ts) = TyApp c $ apply s ts
  ftv (TyMeta a)   = Set.singleton a
  ftv (TyApp _ ts) = ftv ts

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable a => Substitutable (Map.Map k a) where
  apply = Map.map . apply
  ftv = ftv . Map.elems
