module ParserCore where 
    import Ast () 
    import Data.Char (ord)
    
    -- Parser definition. Derived from `https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf`
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
    
