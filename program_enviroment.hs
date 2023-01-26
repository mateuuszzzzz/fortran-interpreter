module ProgramEnviroment (Stack, get, modify, add, delete) where 

    type Stack = [(String, Float)]

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