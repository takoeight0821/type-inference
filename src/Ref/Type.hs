module Ref.Type where

import Data.IORef

data Type = TyMeta (IORef (Maybe Type))
          | TyApp TyCon [Type]
  deriving (Eq)

instance Show Type where
  show (TyMeta _) = "<meta>"
  show (TyApp c ts) = show c <> " " <> show ts

data TyCon = ArrowC | IntC | BoolC
  deriving (Eq, Show)

data Scheme = Forall [IORef (Maybe Type)] Type
  deriving (Eq)

instance Show Scheme where
  show (Forall as t) = "Forall " <> show (length as) <> " " <> show t
