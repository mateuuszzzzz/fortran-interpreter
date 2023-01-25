module Base (Exp (Constant, Variable, Minus, Greater, Times, Div, Plus, Less, Equal), Com (Assign, Seq, Cond, While, Declare, Print)) where 

-- Define State monad and all necessary instances

newtype StateMonad a = St { runState :: Stack -> (a ,Stack, String) } -- Derived from https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State

unwrap :: StateMonad a -> Stack -> (a, Stack, String)
unwrap (St f) = f

instance Functor StateMonad where 
    fmap f (St x) = St $ \m -> let (a, m', s) = x m in (f a, m', s)

instance Applicative StateMonad where 
    pure x = St $ \m -> (x, m, "")
    (<*>) :: StateMonad (a -> b) -> StateMonad a -> StateMonad b
    (<*>) = liftA2 id 
        where liftA2 f x = (<*>) (fmap f x) -- trick to overcome the problem with invisibility of liftA2

instance Monad StateMonad where 
    e >>= f = St $ \m -> let (a, m1, s1) = unwrap e  m
                             (b, m2, s2) = unwrap (f a) m1
                             in (b, m2, s1++s2)


-- AST of language 

data Exp =  Constant Int
            | Variable String
            | Minus Exp Exp
            | Greater Exp Exp
            | Times Exp Exp
            | Div Exp Exp
            | Plus Exp Exp 
            | Less Exp Exp 
            | Equal Exp Exp 
            deriving Show

data Com =  Assign String Exp
            | Seq Com Com
            | Cond Exp Com Com
            | While Exp Com
            | Declare String Exp Com
            | Print Exp
            deriving Show 

-- Enviroment

type Location = Int
type Index = [String]
type Stack = [Int]
 
position :: String -> Index -> Location
position name index = let
                        pos n (nm:nms) = if name == nm
                            then n
                            else pos (n+1) nms
                    in pos 1 index

fetch :: Location -> Stack -> Int
fetch n (v:vs)   =  if n == 1 then v else fetch (n-1) vs


put :: Location -> Int -> Stack -> Stack 
put n x (v:vs) = if n==1
        then x:vs
        else v:(put (n-1) x vs)

-- Monadic enviroment 
getfrom   :: Location -> StateMonad Int 
getfrom i = St (\m -> (fetch i m, m, ""))

write :: Location -> Int -> StateMonad ()
write i v = St (\m -> ( (), put i v m, ""))

push :: Int -> StateMonad ()
push x = St(\m -> ((), x:m, ""))

pop :: StateMonad ()
pop = St (\m -> let  (x:xs) = m in   ( () , xs ,"" ))


-- Evaluator of simple expression that can be reduced to a value 
eval :: Exp -> Index -> StateMonad Int
eval exp index = case exp of
    Constant n -> return n

    Variable x -> let loc = position x index in getfrom loc

    Minus x y -> do 
        a <- eval x index
        b <- eval y index
        return (a-b) 

    Greater x y -> do
        a <- eval x index ;
        b <- eval y index ;
        return (if a > b then 1 else 0)
        
    Times x y -> do 
        a <- eval x index ;
        b <- eval y index ;
        return ( a * b )


-- printing
output :: Show a => a -> StateMonad ()
output v = St $ \m -> ((), m, show v)

interpret :: Com -> Index -> StateMonad ()
interpret stmt index = case stmt of 
    Assign name e-> let loc = position name index in do 
        v <- eval e index
        write loc v
    Seq s1 s2 -> do 
        x <- interpret s1 index
        y <- interpret s2 index
        return ()
    Cond e s1 s2 -> do 
        x <- eval e index
        if x == 1
        then interpret s1 index
        else interpret s2 index

    While e b -> let loop () = do {
        v <- eval e index;
        if v==0 then return ()
        else do {
            interpret b index;
            loop()
        }
    } in loop()
    Declare nm e stmt -> do
        v <- eval e index
        push v
        interpret stmt (nm:index)
        pop
    Print e -> do
        v <- eval e index
        output v

test :: Exp -> (Int, Stack, String)
test a = unwrap (eval a []) []

test2 a = unwrap (interpret a []) []


test_ast = Declare "x" (Constant 150) (Declare "y" (Constant 200) (Seq (While (Greater (Variable "x") (Constant 0)) (Seq (Assign "x" (Minus (Variable "x") (Constant 1) ) ) (Assign "y" (Minus (Variable "y") (Constant 1) ) ) ) ) (Print (Variable "y")) ) )

