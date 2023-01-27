module Parser (languageParser) where
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
        return (Constant ( read [x] :: Float))

    -- Numbers
    digiti :: Parser Exp 
    digiti = do 
        p <- digit
        l <- many digit;
        return (foldl (\a b -> let Constant nra = a
                                   Constant nrb = b 
                                   in Constant (10*nra + nrb)) (Constant 0) (p:l)
                                )
    -- Expression with logic operator

    lexp :: Parser Exp
    lexp = rexp `chainl1` logicop
    
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
        n <- lexp
        symbol ")"
        return n

    -- Arithmetic operators
    addop :: Parser (Exp -> Exp -> Exp)
    addop = do { symbol "-" ; return Minus } +++ do  do { symbol "+" ; return Plus }

    mulop :: Parser (Exp -> Exp -> Exp)
    mulop = do { symbol "*"; return Times} +++ do { symbol "/"; return Div}

    relop :: Parser (Exp -> Exp -> Exp)
    relop = do { symbol ">"; return Greater} +++ do { symbol "<"; return Less} +++ do { symbol "="; return Equal}

    logicop :: Parser (Exp -> Exp -> Exp)
    logicop = do { symbol "||"; return Or} +++ do {symbol "&&"; return And}


    -- Statements
    jump :: Parser Stmt
    jump = do 
        symbol "jump:"
        label <- identif
        return $ Jump label

    label :: Parser Stmt
    label = do 
        symbol "label:"
        label <- identif
        return $ Label label

    printe :: Parser Stmt
    printe = do 
        symbol "print"
        x <- lexp 
        return $ Print x

    reade :: Parser Stmt
    reade = do 
        symbol "read"
        var <- identif 
        return $ Read var

    assign :: Parser Stmt
    assign = do  
        x <- identif 
        symbol ":="
        e <- lexp
        return $ Assign x e

    seqv :: Parser Stmt 
    seqv = do
        symbol "{"
        c <- com
        symbol ";"
        d <- com
        symbol "}"
        return $ Seq c d

    armif :: Parser Stmt
    armif = do
        symbol "if"
        e <- lexp
        symbol "negative"
        n <- com
        symbol "zero"
        z <- com
        symbol "positive"
        p <- com
        return $ ArmIf e n z p

    doloop :: Parser Stmt 
    doloop = do
        symbol "Do"
        symbol "["
        x <- identif
        symbol "="
        e <- lexp
        symbol ";"
        end <- lexp
        symbol ";"
        step <- digiti
        symbol "]"
        symbol "then"
        c <- com
        return $ DoLoop x e end step c


    float = do
        symbol "float"
        x <- identif
        symbol "="
        e <- expr;
        symbol ";"
        c <- com
        return (Float x e c)

    com :: Parser Stmt 
    com = assign +++ seqv +++ float +++ printe +++ reade +++ label +++ jump +++ doloop +++ armif


    languageParser :: String -> Stmt 
    languageParser str = if length result == 0 then error "Syntax error" else fst . head $ result
        where result = (parse com str)

    -- test_str = "{x:=10;y:=20}"
    
    -- test_arm_if = "IFF 0>1 negative print (0-1) zero print 0 positive print 1"

    -- test_jump = "declare n = 1;{ read n; { label: x; IFF n negative {print n; {n:=n+1; jump: x}} zero print n positive {print n; {n:=n-1; jump: x}} }}"
    -- -- test_str2 = "declare x = 150 in print x"

    -- -- test_str3 ="declare x = 150 in declare y = 200 in {while ((x+1 > 0) || (y-10) > 0) && (x*x > 20) do jump: y; label: y}"

    -- -- test_parser = do 
    -- --     symbol "declare"

    -- run_test = parse com test_jump

    -- test_doloop = "Do [ x=10; 20; 100] then print x"


    -- ast = [(ArmIf (Greater (Variable "x") (Constant 0)) (Print (Minus (Times (Constant 2) (Constant 4)) (Variable "x"))) (Print (Variable "x")) (Print (Variable "x")),"")]