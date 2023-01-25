
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
-- Helper data types, interfaces etc.  

-- State monad
newtype State a = St { runState :: (Stack -> (a, Stack, String)) } -- Derived from https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State

-- Helper function to unwrap function nested inside of a Monad
unwrap :: State a -> Stack -> (a, Stack, String)
unwrap (St f) = f

-- Implement `Functor` instance
instance Functor State where 
    fmap f (St x) = St $ \m -> let (a, m', s) = x m in (f a, m', s)

-- Implement `Applicative` instance
instance Applicative State where 
    pure x = St $ \m -> (x, m, "")
    (St f) <*> (St x) = St $ \m -> let (f', m1, s1) = f m in 
                                   let (x', m2, s2) = x m1 in 
                                   (f' x', m2, s1 ++ s2)

-- Implement `Monad` instance
instance Monad State where 
    (St x) >>= f = St $ \m -> let (a, m1, s1) = x m
                                  (b, m2, s2) = unwrap (f a) m1
                                  in (b, m2, s1++s2)


-- This always evaluates to a value (perhaps `Null`)
data SimpleExpr = Constant Value
                  | Variable Id
                  | Minus SimpleExpr SimpleExpr 
                  | Greater SimpleExpr SimpleExpr 
                  | Times SimpleExpr SimpleExpr 
                  deriving Show 

-- Those are expressions that can't be "expressed" as values
data ComplexExpr = Assign String SimpleExpr
                   | Seq ComplexExpr ComplexExpr
                   | Cond SimpleExpr ComplexExpr ComplexExpr
                   | While SimpleExpr ComplexExpr
                   | Declare Id SimpleExpr ComplexExpr
                   | Print SimpleExpr
                   deriving Show 

-- Enviroment data
type  Id = String 
data Value = Numval Int | Null | BoolType Bool
    deriving Show

type MemAddr = Int 
type SymbolsTable = [Id]
type Stack = [Value]

-- Non-monadic helper functions for reading and writing 
getMemAddr :: Id -> SymbolsTable -> MemAddr
getMemAddr id index = let 
        pos n (nm:nms) = if id == nm then n else pos (n+1) nms 
        in pos 1 index

readMem :: MemAddr -> Stack -> Value 
readMem addr (x:xs) = if addr == 1 then x else readMem (addr-1) xs

writeToMem :: MemAddr -> Value -> Stack -> Stack 
writeToMem addr val (x:xs) = if addr == 0 then val:xs else val:(writeToMem (addr-1) val xs)

-- Monadic wrappers 
getFrom :: MemAddr -> State Value 
getFrom addr = St $ \m -> (readMem addr m, m, "")

putTo :: MemAddr -> Value -> State ()
putTo addr val = St $ \m -> ((), writeToMem addr val m, "")

push :: Value -> State ()
push val = St $ \m -> ((), val:m, "")

pop :: State ()
pop = St $ \(m:ms) -> ((), ms, "") 

-- helper function for printing 
handlePrint v = St $ \m -> ((), m, show v)

nullable :: Value  -> Bool
nullable Null = True
nullable _ = False

-- AST does not store haskell's built-in types. Values are wrapped in `Value` type. This requires additional handling
getNumVal :: Value -> Int
getNumVal (Numval a) = a
getNumVal (BoolType True) = 1
getNumVal (BoolType False) = 0
getNumVal (Null) = 0 -- guarantees that `if (null)` statement has proper evaluation

evalSimple :: SimpleExpr -> SymbolsTable -> State Value
evalSimple expr symbols = case expr of
                Constant n -> return n 
                Variable x -> getFrom (getMemAddr x symbols)
                Minus x y -> do {
                    a <- evalSimple x symbols
                    ;b <- evalSimple y symbols
                    ;return (if nullable a || nullable b then Null else (Numval (getNumVal(a) + getNumVal(b))))
                }
                Greater x y -> do { -- for nullable values I assume that they're not comparable (comparition results in false)
                    a <- evalSimple x symbols
                    ;b <- evalSimple y symbols
                    ;return (if nullable a || nullable b then BoolType False else (if getNumVal(a) > getNumVal(b) then BoolType True else BoolType False))
                }
                Times x y -> do {
                    a <- evalSimple x symbols
                    ;b <- evalSimple y symbols
                    ;return (if nullable a || nullable b then Null else (Numval (getNumVal(a) * getNumVal(b))))
                }

evalComplex :: ComplexExpr -> SymbolsTable -> State ()
evalComplex complexExpr symbols = case complexExpr of 
                Assign id expr -> do {
                    val <- evalSimple expr symbols
                    ; putTo (getMemAddr id symbols) val
                }
                Cond expr exec1 exec2 -> do {
                    val <- evalSimple expr symbols
                    ;(if getNumVal(val) /= 0 then evalComplex exec1 symbols else evalComplex exec2 symbols)
                }
                Seq exec1 exec2 -> do {
                    res1 <- evalComplex exec1 symbols
                    ;res2 <- evalComplex exec2 symbols
                    ;return ()
                }
                While expr exec -> let while () = do {
                    val <- evalSimple expr symbols
                    ;(if getNumVal(val) == 0 then return () else do {
                        evalComplex exec symbols
                        ; while ()
                    })
                } in while()
                Declare id expr exec -> do { -- execution scope (last arguments) allows us to push and pop from stack properly
                    val <- evalSimple expr symbols
                    ;push val
                    ;evalComplex exec (id:symbols)
                    ;pop
                }
                Print expr -> do {
                    val <- evalSimple expr symbols
                    ;handlePrint val
                }


interpretSimpleExpr program = unwrap (evalSimple program []) []
interpretComplexExpr program = unwrap (evalComplex program []) []