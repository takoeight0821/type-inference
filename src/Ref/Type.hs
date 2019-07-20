module Ref.Type where

import Data.IORef

data Type = TyMeta (IORef (Maybe Type))
          | TyApp TyCon [Type]
  deriving (Eq)

instance Show Type where
  show (TyMeta _) = "<meta>"
  show (TyApp c ts) = show c <> " " <> show ts

data TyCon = ArrowC | IntCon | BoolCon
  deriving (Eq, Show)

data Scheme = Scheme [String] Type
  deriving (Eq, Show)
