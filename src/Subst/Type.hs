module Subst.Type where

import qualified Data.Set as Set

type TyVar = Int

data Type = TyMeta TyVar
          | TyApp TyCon [Type]
  deriving (Eq, Show)

data TyCon = ArrowC | IntC | BoolC
  deriving (Eq, Show)

data Scheme = Forall [TyVar] Type
  deriving (Eq, Show)

tyvars :: Type -> Set.Set TyVar
tyvars (TyMeta v) = Set.singleton v
tyvars (TyApp _ ts) = Set.unions $ map tyvars ts
