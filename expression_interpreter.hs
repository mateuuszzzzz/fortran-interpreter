module ExpressionInterpreter (evalExpression) where 
    import Ast
    import ProgramEnviroment

    evalExpression :: Exp -> Stack -> Float
    evalExpression (Constant n) _ = n

    evalExpression (Variable x) env = get x env

    evalExpression (Minus exp1 exp2) env = a - b
        where a = evalExpression exp1 env
              b = evalExpression exp2 env

    evalExpression (Greater exp1 exp2) env = a > b 
        where a = evalExpression exp1 env
              b = evalExpression exp2 env

    evalExpression (Times exp1 exp2) env = a * b 
        where a = evalExpression exp1 env
              b = evalExpression exp2 env 

    evalExpression (Div exp1 exp2) env = a / b 
        where a = evalExpression exp1 env
              b = evalExpression exp2 env 

    evalExpression (Plus exp1 exp2) env = a + b 
        where a = evalExpression exp1 env
              b = evalExpression exp2 env
    
    evalExpression (Less exp1 exp2) env = if a < b then 1.0 else 0.0
        where a = evalExpression exp1 env
              b = evalExpression exp2 env 

    evalExpression (Equal exp1 exp2) env = if a == b then 1.0 else 0.0
        where a = evalExpression exp1 env
              b = evalExpression exp2 env

    evalExpression (Or exp1 exp2) env = if a != 0 || b != 0 then 1.0 else 0.0
        where a = evalExpression exp1 env
              b = evalExpression exp2 env

    evalExpression (And exp1 exp2) env = if a != 0 && b != 0 then 1.0 else 0.0
        where a = evalExpression exp1 env
              b = evalExpression exp2 env
