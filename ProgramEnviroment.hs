module ProgramEnviroment (Stack, get, modify, add, delete, NextInstruction, InputAction, OutputAction, Termination, Env) where 
    import PrettyPrinting

    type Stack = [(String, Float)] -- Current stack of a program
    type NextInstruction = Int -- Address of the next instruction
    type InputAction = Maybe String -- != Nothing if program demands input from stdin
    type OutputAction = Maybe String -- != Nothing if program demands output to stdout
    type Termination = Bool -- True iff interpreter reached `ENDPROG`
    type Env = (Instructions, NextInstruction, Stack, InputAction, OutputAction, Termination)

    get :: String -> Stack -> Float
    get var [] = error "Variable not in scope"
    get var (x:xs) = if fst x == var then snd x else get var xs

    modify :: String -> Float -> Stack -> Stack
    modify var val [] = error "Variable not in scope"
    modify var val (x:xs)
                    | fst x == var = (var, val):xs
                    | otherwise = x:(modify var val xs)

    add :: String -> Float -> Stack -> Stack
    add var val env = (var, val):env

    delete :: String -> Stack -> Stack
    delete var [] = error "Variable not in scope"
    delete var (x:xs) = if fst x == var then xs else x:(delete var xs)