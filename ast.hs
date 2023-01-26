module Ast (Exp (Constant, Variable, Minus, Greater, Times, Div, Plus, Less, Equal, Or, And), Com (Assign, Seq, Cond, ArmIf, While, Declare, Print, Jump, Label, DoLoop, Read)) where 
-- This file defines AST of the language 

data Exp =  Constant Float
            | Variable String
            | Minus Exp Exp
            | Greater Exp Exp
            | Times Exp Exp
            | Div Exp Exp
            | Plus Exp Exp 
            | Less Exp Exp 
            | Equal Exp Exp 
            | Or Exp Exp 
            | And Exp Exp
            deriving Show

data Com =  Assign String Exp -- OK
            | Seq Com Com -- OK
            | Cond Exp Com Com -- OK
            | ArmIf Exp Com Com Com -- OK
            | While Exp Com -- OK
            | DoLoop String Exp Exp Exp Com
            | Declare String Exp Com -- OK
            | Print Exp -- OK
            | Jump String --OK
            | Label String --OK
            | Read String -- OK
            deriving Show