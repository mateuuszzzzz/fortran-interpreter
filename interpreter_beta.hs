module Interpreter where 
    import ProgramEnviroment 
    import ExpressionInterpreter
    import Ast
    import PrettyPrinting
    import System.IO 
    
    eval :: Program -> Instructions -> Stack -> IO ()
    eval _ _ _ = putStr "x"



    main :: IO ()
    main = do 
        contents <- readFile "program.txt"
        putStr show . astToInstructions . languageParser $ contents 