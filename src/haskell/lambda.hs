module Lambda (Term(Var, Lam, App), subst, alpha, beta) where

import qualified Data.Set as Set

-- Lambda calculus, as presented by Alonzo Church.
type Variable = String

data Term = Var Variable        -- named variable
          | Lam Variable Term   -- lambda abstraction
          | App Term Term       -- application
          deriving Show

-- find all free variables in a term
freevars :: Term -> Set.Set Variable
freevars (Var x) = Set.singleton x
freevars (Lam x t) = Set.delete x (freevars t)
freevars (App m n) = Set.union (freevars m) (freevars n)

-- create a new variable from an existing one (by appending an apostrophe)
fresh :: Variable -> Variable
fresh x = x ++ "'"

-- capture-avoiding substitution: T[x := Y]
subst :: Variable   -- variable to substitute for (x)
      -> Term       -- term to substitute with variable (Y)
      -> Term       -- term to perform substitution on (T)
      -> Term
subst x y (Var v) | v == x    = y
                  | otherwise = (Var v)
subst x y (App m n) = App (subst x y m) (subst x y n)
subst x y (Lam v t) | v == x                    = Lam v t
                    | Set.member v (freevars y) = subst x y (alpha (fresh v) (Lam v t))
                    | otherwise                 = Lam v (subst x y t)

-- alpha-renaming
alpha :: Variable   -- new variable
      -> Term
      -> Term
alpha x (Lam v t) | Set.member x (freevars t)   = alpha (fresh x) (Lam v t)
                  | otherwise                   = Lam x (subst v (Var x) t)

-- beta reduction
beta :: Term -> Term
beta (App (Lam v t) x) = subst v x t
beta (App (App m n) p) = App (beta (App m n)) p
