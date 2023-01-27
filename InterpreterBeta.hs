{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use isJust" #-}
module Interpreter where 
    import ProgramEnvironment 
    import System.Environment 
    import ExpressionInterpreter
    import Ast
    import PrettyPrinting
    import System.IO 
    import Parser 

    -- Helper to get right instruction
    getInstruction :: Int -> Instructions -> Instruction 
    getInstruction idx [] = error "Internal error"
    getInstruction idx (x:xs) = if fst x == idx then x else getInstruction idx xs 

    findAddrWithLabel :: String -> Instructions -> Int
    findAddrWithLabel label [] = error "Label does not exist"
    findAddrWithLabel label ((addr, LABEL label_):xs) = if label == label_ then addr else findAddrWithLabel label xs
    findAddrWithLabel label (_:xs) = findAddrWithLabel label xs



    evalRegister :: Env -> IO Env 
    evalRegister (instructions, nextInstruction, stack, _, _, x) = case getInstruction nextInstruction instructions of
                (_, ENDPROG) -> return ([], 0, [], Nothing, Nothing, True) -- termination of a program

                (addr, STACK_PUSH name exp) -> let value = evalExpression exp stack -- Stores new variable in memory
                                                   modifiedStack = add name value stack
                                               in return (instructions, addr+1, modifiedStack, Nothing, Nothing, False)

                (addr, STACK_MODIFY name exp) -> let value = evalExpression exp stack -- Modifies existing variable
                                                     modifiedStack = modify name value stack
                                               in return (instructions, addr+1, modifiedStack, Nothing, Nothing, False)

                (addr, STACK_DELETE name) -> let modifiedStack = delete name stack -- Deletes value from the stack
                                            in return (instructions, addr+1, modifiedStack, Nothing, Nothing, False)

                (addr, PRINT exp) -> let value = show (evalExpression exp stack) -- this will use putStr in main
                                         in return (instructions, addr+1, stack, Nothing, Just value, False)

                (addr, READ var) -> return (instructions, addr+1, stack, Just var, Nothing, False) -- Read value from stdin

                (_, ARMIF exp neg zero pos) -> let value = evalExpression exp stack -- Arithmetic if 
                                                   addr 
                                                        | value == 0 = zero
                                                        | value > 0 = pos
                                                        | otherwise = neg 
                                                    in return (instructions, addr, stack, Nothing, Nothing, False)

                (_, IF exp true false) -> let value = evalExpression exp stack -- Internal IF
                                              addr = if value == 0 then false else true
                                                in return (instructions, addr, stack, Nothing, Nothing, False)

                (addr, LABEL _) -> return (instructions, addr+1, stack, Nothing, Nothing, False) -- Label can be skipped

                (_, JUMP label) -> let addr = findAddrWithLabel label instructions -- Finds address of register with a given label
                                        in return (instructions, addr, stack, Nothing, Nothing, False)

                (_, ELSE addr) -> return (instructions, addr, stack, Nothing, Nothing, False) -- addr is next register that should be executed

                (_, ZEROIF addr) -> return (instructions, addr, stack, Nothing, Nothing, False) -- same here

                (_, POSITIVEIF addr) -> return (instructions, addr, stack, Nothing, Nothing, False) -- same here



    interpreter_loop :: (Instructions, NextInstruction, Stack, Maybe String, Maybe String, Termination) -> IO ()
    interpreter_loop args = do 
        (instructions, nextInstruction, stack, inputAction, outputAction, terminate) <- evalRegister args
        if terminate -- Program terminated
            then 
                return ()
            else
                if outputAction /= Nothing -- Program requested IO operation
                    then let Just outputStr = outputAction in do
                        putStrLn outputStr
                        interpreter_loop (instructions, nextInstruction, stack, Nothing, Nothing, terminate)
                    else
                        if inputAction /= Nothing -- Program requested IO operation
                            then let Just inputStr = inputAction in do 
                                val <- getLine
                                interpreter_loop (instructions, nextInstruction, modify inputStr (read val :: Float) stack, Nothing, Nothing, terminate) -- Here we modify stack with value from input
                            else 
                                interpreter_loop (instructions, nextInstruction, stack, Nothing, Nothing, terminate)

                 
    main :: IO ()
    main = do 
        [file] <- getArgs 
        contents <- readFile file 
        -- mapM_ putStrLn (map show (astToInstructions(languageParser(contents)))) -- shows list of registers for a given program
        interpreter_loop (astToInstructions . languageParser $ contents, 0, [], Nothing, Nothing, False)