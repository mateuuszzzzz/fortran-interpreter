{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use isJust" #-}
module Interpreter where 
    import ProgramEnviroment 
    import ExpressionInterpreter
    import Ast
    import PrettyPrinting
    import System.IO 
    import Parser 

    -- Helper to get right instruction
    getInstruction :: Int -> Instructions -> Instruction 
    getInstruction idx [] = error "Internal error"
    getInstruction idx (x:xs) = if fst x == idx then x else getInstruction idx xs 


    evalRegister :: Env -> IO Env 
    evalRegister (instructions, nextInstruction, stack, _, _, x) = case getInstruction nextInstruction instructions of
                (_, ENDPROG) -> return ([], 0, [], Nothing, Nothing, True) -- termination of a program
                (addr, STACK_PUSH name exp) -> let value = evalExpression exp stack -- Stores new variable in memory
                                                   modifiedStack = add name value stack
                                               in return (instructions, addr+1, modifiedStack, Nothing, Nothing, False)
                (addr, STACK_MODIFY name exp) -> let value = evalExpression exp stack -- Modifies existing variable
                                                     modifiedStack = modify name value stack
                                               in return (instructions, addr+1, modifiedStack, Nothing, Nothing, False)
                (addr, STACK_DELETE name) -> let modifiedStack = delete name stack
                                            in return (instructions, addr+1, modifiedStack, Nothing, Nothing, False)
                (addr, PRINT exp) -> let value = show (evalExpression exp stack) -- this will use putStr in main
                                         in return (instructions, addr+1, stack, Nothing, Just value, False)

    -- TO DO: W instrukcjach typu IF THEN ELSE (to samo dla arytmetycznego) trzeba dorobić argumenty w strukturze `RegisterOP`, które dają informację gdzie zrobić doskok. Bo wewnątrz tego ifa mogą być inne zagniezdzone
    -- Więc najblizszy THEN ALBO ELSE nie wystarczy

    interpreter_loop :: (Instructions, NextInstruction, Stack, Maybe String, Maybe String, Termination) -> IO ()
    interpreter_loop args = do 
        (instructions, nextInstruction, stack, inputAction, outputAction, terminate) <- evalRegister args
        if terminate
            then 
                return ()
            else
                if outputAction /= Nothing
                    then let Just outputStr = outputAction in do
                        putStrLn outputStr
                        interpreter_loop (instructions, nextInstruction, stack, Nothing, Nothing, terminate)
                    else
                        if inputAction /= Nothing
                            then let Just inputStr = inputAction in do 
                                val <- getLine
                                interpreter_loop (instructions, nextInstruction, modify inputStr (read val :: Float) stack, Nothing, Nothing, terminate) -- Here we modify stack with value from input
                            else 
                                interpreter_loop (instructions, nextInstruction, stack, Nothing, Nothing, terminate)

                 
                

    main :: IO ()
    main = do 
        contents <- readFile "program.txt"
        interpreter_loop (instructions, 0, [], Nothing, Nothing, False)
            where instructions = astToInstructions . languageParser $ contents