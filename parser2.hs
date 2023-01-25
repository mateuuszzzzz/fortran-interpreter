import Helpers (isAlpha, isDigit)
import Base
import Data.Char (ord)
import Debug.Trace
-- Define parser 
-- From `Monadic Parser Combinators` by Graham Hutton
-- Empty list indicates failure and singleton list success 
newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a,String)]
parse (Parser p) = p 


-- Instances of required classes 

instance Functor Parser where 
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser x) = Parser $ \m -> [ (f a, m') | (a, m') <- x m]

instance Applicative Parser where 
    pure :: a -> Parser a
    pure x = Parser $ \m -> [(x, m)]
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) = liftA2 id 
        where liftA2 f x = (<*>) (fmap f x) -- trick to overcome the problem with invisibility of liftA2

instance Monad Parser where
 p >>= f  = Parser (\m ->  concat [ parse (f a) cs' | (a,cs') <- parse p m] )


-- MonadPlus 
class (Monad m) => MonadPlus m where
   mzero :: m a
   mplus :: m a -> m a -> m a

instance MonadPlus Parser where 
     mzero =  Parser $ \m -> []
     p `mplus` q = Parser(\m -> parse p m ++ parse q m)

-- Parser combinators 

item :: Parser Char -- OK
item = Parser $ \xs -> case xs of
    "" -> []
    (c:cs) -> [(c,cs)]

(+++) :: Parser a -> Parser a -> Parser a --OK
p +++ q = Parser $ \m -> case parse (p `mplus` q) m of
    [] -> []
    (x:xs) -> [x]

sat :: (Char -> Bool) -> Parser Char --OK
sat p = do
    c <- item
    if p c then return c else mzero

(?) :: Parser a -> (a-> Bool) -> Parser a --OK
p ? test = do
    c <- p 
    if test c then return c else mzero 

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
space = many (sat $ \c -> c=='\n' || c=='\t' || c=='\r' || c=='\f' || c=='\v')

token :: Parser a -> Parser a
token p = do
    a <- p
    space
    return a

symbol :: String -> Parser String
symbol cs = token (string cs)

apply :: Parser a -> String -> [(a,String)]
apply p = parse $ do 
        space
        p 


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
    return $ Declare (x e c )

com :: Parser Com 
com = assign +++ seqv +++ cond +++ while +++ declare +++ printe 


test_str = "{x:=10;y:=20}"

test_str2 = "declare x = 150 in print x"


run_test = parse seqv test_str2