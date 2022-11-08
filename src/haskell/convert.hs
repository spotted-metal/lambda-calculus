module Convert where

import Lambda(Term(Var, Lam, App))
import DeBruijn(Term(Var, Lam, App))

-- convert lambda terms using named variables to using de Bruijn indices
-- TODO: function is currently a stub
convertLdB :: Lambda.Term -> deBruijn.Term
convertLdB (Lambda.Var x) = deBruijn.Var 0
convertLdB (Lambda.App x y) = deBruijn.App (convertLdB x) (convertLdB y)
convertLdB (Lambda.Lam x t) = deBruijn.Lam (convertLdB t)
