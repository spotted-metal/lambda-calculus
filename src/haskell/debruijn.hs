module DeBruijn(Term(Var, Lam, App), subst, alpha, beta) where

-- Lambda calculus using de Bruijn indices.
type Variable = Integer

data Term = Var Variable    -- named variable
          | Lam Term        -- lambda abstraction
          | App Term Term   -- application
          deriving Show

-- increment all free variables by one
incfree :: Term -> Term
incfree t = incfreefrom 0 t

incfreefrom :: Variable -> Term -> Term
incfreefrom n (Var m) | m >= n    = Var (m+1)
                      | otherwise = Var m
incfreefrom n (App y z) = App (incfreefrom n y) (incfreefrom n z)
incfreefrom n (Lam t) = Lam (incfreefrom (n+1) t)

-- capture-avoiding substitution
subst :: Variable -> Term -> Term -> Term
subst n x (Var m) | m == n    = x
                  | otherwise = Var m
subst n x (App y z) = App (subst n x y) (subst n x z)
subst n x (Lam t) = Lam (subst (n+1) (incfree x) t)

-- alpha-renaming
-- Using de Bruijn indices, alpha-equivalent terms are syntactically equal.
alpha :: Term -> Term
alpha (Lam t) = Lam (subst 0 (Var 0) t) -- alpha = id

-- beta-reduction
beta :: Term -> Term
beta (App (Lam t) x) = subst 0 x t
