module Ast (Exp (Constant, Variable, Minus, Greater, Times, Div, Plus, Less, Equal), Com (Assign, Seq, Cond, ArmIf, While, Declare, Print)) where 
-- This file defines AST of the language 

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

data Com =  Assign String Exp -- OK
            | Seq Com Com -- OK
            | Cond Exp Com Com -- OK
            | ArmIf Exp Com Com Com -- OK
            | While Exp Com -- OK
            | Declare String Exp Com -- OK
            | Print Exp -- OK
            | Jump String
            | Label String
            deriving Show