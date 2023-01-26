module Parser where
    import Data.Char (ord)
    import ParserCore
    import Ast 
    import Helpers (isDigit, isAlpha)
    
    -- Bulding grammar of language 

    -- Variables
    var :: Parser Exp
    var = do Variable <$> identif

    -- Digits
    digit :: Parser Exp 
    digit = do 
        x <- token (sat isDigit) 
        return (Constant ( ord x - ord '0'))

    -- Numbers
    digiti :: Parser Exp 
    digiti = do 
        p <- digit
        l <- many digit;
        return (foldl (\a b -> let Constant nra = a
                                   Constant nrb = b 
                                   in Constant (10*nra + nrb)) (Constant 0) (p:l)
                                )
    -- Expression with relational operator
    rexp :: Parser Exp 
    rexp = expr `chainl1` relop

    --Expression with additive operator
    expr :: Parser Exp
    expr = term `chainl1` addop

    --Expression with multiplicative operator
    term :: Parser Exp 
    term = factor `chainl1` mulop 

    -- Parser for factor 
    factor :: Parser Exp 
    factor = var +++ digiti +++ do 
        symbol "("
        n <- rexp
        symbol ")"
        return n

    -- Arithmetic operators
    addop :: Parser (Exp -> Exp -> Exp)
    addop = do { symbol "-" ; return Minus } +++ do  do { symbol "+" ; return Plus }

    mulop :: Parser (Exp -> Exp -> Exp)
    mulop = do { symbol "*"; return Times} +++ do { symbol "/"; return Div}

    relop :: Parser (Exp -> Exp -> Exp)
    relop = do { symbol ">"; return Greater} +++ do { symbol "<"; return Less} +++ do { symbol "="; return Equal}


    -- Commands 
    printe :: Parser Com 
    printe = do 
        symbol "print"
        x <- rexp 
        return $ Print x

    assign :: Parser Com 
    assign = do  
        x <- identif 
        symbol ":="
        e <- rexp
        return $ Assign x e

    seqv :: Parser Com 
    seqv = do
        symbol "{"
        c <- com
        symbol ";"
        d <- com
        symbol "}"
        return $ Seq c d


    cond :: Parser Com 
    cond = do 
        symbol "if"
        e <- rexp 
        symbol "then"
        c <- com
        symbol "else"
        d <- com
        return $ Cond e c d 

    armif :: Parser Com
    armif = do
        symbol "IFF"
        e <- rexp
        symbol "negative"
        n <- com
        symbol "zero"
        z <- com
        symbol "positive"
        p <- com
        return $ ArmIf e n z p

    while :: Parser Com 
    while = do 
        symbol "while"
        e <- rexp 
        symbol "do"
        c <- com 
        return $ While e c

    declare = do
        symbol "declare"
        x <- identif
        symbol "="
        e <- rexp 
        symbol "in"
        c <- com
        return (Declare x e c)

    com :: Parser Com 
    com = assign +++ seqv +++ cond +++ while +++ declare +++ printe 


    test_str = "{x:=10;y:=20}"

    test_str2 = "declare x = 150 in print x"

    test_str3 ="declare x = 150 in declare y = 200 in {while x > 0 do { x:=x-1; y:=y-1 }; print y}"

    test_parser = do 
        symbol "declare"

    run_test = parse com test_str3


    ast = [(ArmIf (Greater (Variable "x") (Constant 0)) (Print (Minus (Times (Constant 2) (Constant 4)) (Variable "x"))) (Print (Variable "x")) (Print (Variable "x")),"")]