module Syntax where

data Exp = Var String
         | App Exp Exp
         | Const Const
         | Lam String Exp
         | Let String Exp Exp
  deriving (Show, Eq)

data Const = IntC Integer
           | BoolC Bool
  deriving (Show, Eq)
