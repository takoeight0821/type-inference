module Syntax where

data Exp = Var String
         | App Exp Exp
         | Lam String Exp
         | Let String Exp Exp
  deriving (Show, Eq)
