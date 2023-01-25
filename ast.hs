module Ast (Exp (Constant, Variable, Minus, Greater, Times, Div, Plus, Less, Equal), Com (Assign, Seq, Cond, While, Declare, Print)) where 
-- This file defines AST of language 

data Exp =  Constant Int
            | Variable String
            | Minus Exp Exp
            | Greater Exp Exp
            | Times Exp Exp
            | Div Exp Exp
            | Plus Exp Exp 
            | Less Exp Exp 
            | Equal Exp Exp 
            deriving Show

data Com =  Assign String Exp
            | Seq Com Com
            | Cond Exp Com Com
            | While Exp Com
            | Declare String Exp Com
            | Print Exp
            deriving Show 