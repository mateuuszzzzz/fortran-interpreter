-- Defining a Parser monad
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where 
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser x) = Parser $ \s -> do 
        (x', s') <- x s
        return (f x', s')

instance Applicative Parser where 
    pure :: a -> Parser a
    pure x = Parser $ \s -> Just (x,s)
    (Parser f) <*> (Parser x) = Parser $ \s -> do 
        (f', s1) <- f s 
        (x', s2) <- x s1
        return (f' x', s2)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser x) >>= f = Parser $ \s -> do
        (x', s') <- x s 
        runParser (f x') s'

instance MonadFail Parser where 
    fail :: String -> Parser a
    fail _ = Parser $ const Nothing


class (Applicative f) => Alternative f where 
    empty :: f a
    ( <|> ) :: f a -> f a -> f a 
    some :: f a -> f [a]
    many :: f a -> f [a]
    some v = some_v -- one or more
        where many_v = some_v <|> pure []
              some_v = (:) <$> v <*> many_v 
    many v = many_v -- zero or more
        where many_v = some_v <|> pure []
              some_v = (:) <$> v <*> many_v 


instance Alternative Parser where 
    empty = fail ""
    (Parser x) <|> (Parser y) = Parser $ \s -> 
        case x s of 
            Just x -> Just x
            Nothing -> y s 


-- Define grammar of language 

-- <rexp> ::= <rexp> <relop> <expr> | <expr>
-- <expr> ::= <expr> <addop> <term> | <term>
-- <term> ::= <term> <mulop> <factor> |< factor>
-- <factor> ::= <var> | <digiti> | ( <expr> )
-- <var> ::= <Identifier>
-- <digiti> ::= <digit> | <digit> <digiti>
-- <digit> ::= 0 | 1 | ... | 9
-- <addop> ::= + | -
-- <mulop> ::= * | /
-- <relop> ::= > | < | =

-- <com> ::= <assign> | <seqv> | <cond> | <while> | <declare> | <printe>
-- assign> ::= <
-- identif> ":=" <rexp>
-- <seqv> ::= "{" <com> ";" <com> "}"
-- <cond> ::= "if" <rexp> "then" <com> "else" <com>
-- <while> ::= "while" <rexp> "do" <com>
-- <declare> ::= "declare" <identif> "=" <rexp> "in" <com>
-- <printe> ::= "print" <rexp>


digit :: Parser Char
digit = char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9'

digiti :: Parser [Char]
digiti = some digit

addop :: Parser Char 
addop = char '+' <|> char '-'

mulop :: Parser Char
mulop = char '*' <|> char '/'

relop :: Parser Char 
relop = char '>' <|> char '<' <|> char '='

char :: Char -> Parser Char 
char c = Parser $ \s -> helper s
    where helper [] = Nothing 
          helper (x:xs) | x == c = Just (c, xs)
                        | otherwise = Nothing

string :: String -> Parser String 
string = mapM char 

space :: Parser Char 
space = char ' ' <|> char '\n' <|> char '\r' <|> char '\t'

ss = many space 

parseHW = (,) <$> (string "Hello" <* ss) <*> string "World"
