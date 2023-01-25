module Parser where
    import Data.Char (ord)
    import ParserCore
    import Ast 
    import Helpers (isDigit, isAlpha)
    
    -- Parsers that are used to build more concrete parsers

    char :: Char -> Parser Char --OK
    char c = sat (c==)

    string :: String -> Parser String --OK
    string "" = return "" 
    string (x:xs) = do
        char x
        string xs
        return (x:xs)
    many :: Parser a -> Parser [a] --OK
    many p = manyHelper p +++ return []
        where manyHelper p = do
                a <- p
                as <- many p
                return (a:as)

    space :: Parser String 
    space = many (sat $ \c -> c=='\n' || c=='\t' || c=='\r' || c=='\f' || c=='\v' || c==' ')

    token :: Parser a -> Parser a
    token p = do
        a <- p
        space
        return a

    symbol :: String -> Parser String
    symbol cs = token (string cs)

    -- Parsing ids 
    ident :: Parser [Char]
    ident = do 
        l <- sat isAlpha
        lsc <- many (sat (\a -> isAlpha a || isDigit a))
        return (l:lsc)

    identif :: Parser [Char]
    identif = token ident

    -- Parser for vars 
    var :: Parser Exp
    var = do Variable <$> identif


    chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
    chainl p op a = (p `chainl1` op) +++ return a

    chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
    p `chainl1` op = do { a <- p; rest a }
        where rest a = ( do  
                f <- op
                b <- p
                rest (f a b) 
                ) +++ return a


    -- building grammar 

    digit :: Parser Exp 
    digit = do 
        x <- token (sat isDigit) 
        return (Constant ( ord x - ord '0'))

    digiti :: Parser Exp 
    digiti = do 
        p <- digit
        l <- many digit;
        return (foldl (\a b -> let Constant nra = a
                                   Constant nrb = b 
                                   in Constant (10*nra + nrb)) (Constant 0) (p:l)
                                )
    rexp :: Parser Exp 
    rexp = expr `chainl1` relop

    expr :: Parser Exp
    expr = term `chainl1` addop

    term :: Parser Exp 
    term = factor `chainl1` mulop 

    -- Parser for factor 
    factor :: Parser Exp 
    factor = var +++ digiti +++ do 
        symbol "("
        n <- rexp
        symbol ")"
        return n

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


    ast = Declare "x" (Constant 150) (Declare "y" (Constant 200) (Seq (While (Greater (Variable "x") (Constant 0)) (Seq (Assign "x" (Minus (Variable "x") (Constant 1))) (Assign "y" (Minus (Variable "y") (Constant 1))))) (Print (Variable "y"))))