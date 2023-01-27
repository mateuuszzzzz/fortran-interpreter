module Ast (Exp (Constant, Variable, Minus, Greater, Times, Div, Plus, Less, Equal, Or, And), Stmt (Assign, Seq, ArmIf, Float, Print, Jump, Label, DoLoop, Read)) where 
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

data Stmt =  Assign String Exp -- OK
            | Seq Stmt Stmt -- OK
            | ArmIf Exp Stmt Stmt Stmt-- OK
            | DoLoop String Exp Exp Exp Stmt -- OK
            | Float String Exp Stmt -- OK
            | Print Exp -- OK
            | Jump String --OK
            | Label String --OK
            | Read String -- OK
            deriving Show