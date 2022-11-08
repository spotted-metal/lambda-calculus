module Convert where

import Lambda(Term(Var, Lam, App))
import DeBruijn(Term(Var, Lam, App))

-- convert lambda terms using named variables to using de Bruijn indices
-- TODO: function is currently a stub
convertLdB :: Lambda.Term -> DeBruijn.Term
convertLdB (Lambda.Var x) = DeBruijn.Var 0
convertLdB (Lambda.App x y) = DeBruijn.App (convertLdB x) (convertLdB y)
convertLdB (Lambda.Lam x t) = DeBruijn.Lam (convertLdB t)
