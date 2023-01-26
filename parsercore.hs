module ParserCore (Parser (Parser), parse, item, (+++), sat, char, string, many, space, token, symbol, ident, identif, chainl, chainl1) where 
    import Helpers (isDigit, isAlpha)
    import Data.Char (ord)
    
    -- Parser definition and logic. Mostly derived from `https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf`
    newtype Parser a = Parser (String -> [(a, String)])

    -- Similar behaviour as `Interpreter.unwrap`
    parse :: Parser a -> String -> [(a,String)]
    parse (Parser p) = p 

    -- Parser has to be an instance of a few classes
    instance Functor Parser where 
        fmap :: (a -> b) -> Parser a -> Parser b
        fmap f (Parser x) = Parser $ \m -> [ (f a, m') | (a, m') <- x m]

    instance Applicative Parser where 
        pure :: a -> Parser a
        pure x = Parser $ \m -> [(x, m)]
        (<*>) :: Parser (a -> b) -> Parser a -> Parser b
        (<*>) = liftA2 id 
            where liftA2 f x = (<*>) (fmap f x) -- trick to overcome the problem with invisibility of liftA2, since it cannot be imported

    instance Monad Parser where
        p >>= f  = Parser (\m ->  concat [ parse (f a) cs' | (a,cs') <- parse p m] )


    -- We need suitable choice combinator for parsers
    class (Monad m) => MonadPlus m where
        mzero :: m a
        mplus :: m a -> m a -> m a

    instance MonadPlus Parser where 
        mzero =  Parser $ \m -> []
        p `mplus` q = Parser(\m -> parse p m ++ parse q m)

    -- Essential parsers that can be used to build more sophisticated ones

    -- This parser accepts first character of string
    item :: Parser Char -- OK
    item = Parser $ \str -> case str of
        "" -> []
        (c:cs) -> [(c,cs)]

    -- Sometimes we need to combine a few parsers and get the first succesful parsing (and discard remaining ones)
    (+++) :: Parser a -> Parser a -> Parser a --OK
    p +++ q = Parser $ \str -> case parse (p `mplus` q) str of
        [] -> []
        (x:_) -> [x]

    -- Predicate that allows us to build rules for parsers e.g. using `isDigit :: Char -> Bool` accepts only digits
    sat :: (Char -> Bool) -> Parser Char --OK
    sat p = do
        c <- item
        if p c then return c else mzero

    -- Simple parsers
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
    space = many (sat $ \c -> c=='\n' || c=='\t' || c=='\r' || c=='\f' || c=='\v' || c==' ')  --OK

    token :: Parser a -> Parser a
    token p = do
        a <- p
        space
        return a

    symbol :: String -> Parser String
    symbol cs = token (string cs)

    ident :: Parser [Char]
    ident = do 
        l <- sat isAlpha
        lsc <- many (sat (\a -> isAlpha a || isDigit a))
        return (l:lsc)

    identif :: Parser [Char]
    identif = token ident

    chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
    chainl p op a = (p `chainl1` op) +++ return a

    chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
    p `chainl1` op = do { a <- p; rest a }
        where rest a = ( do  
                f <- op
                b <- p
                rest (f a b) 
                ) +++ return a


    
