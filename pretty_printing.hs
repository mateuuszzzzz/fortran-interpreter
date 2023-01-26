module PrettyPrinting (RegisterOP (STACK_PUSH, WHILE, STACK_MODIFY, PRINT, STACK_DELETE, IF, THEN, ELSE, ARMIF, NEGATIVEIF, ZEROIF, POSITIVEIF, JUMP, LABEL, READ), Instructions, generateCodeFromAST, astToInstructions) where
    import Ast
    
    -- Since this language has goto statements it is quite difficult to operate on the AST.
    -- Also, it would be difficult to create an interpreter that accepts any proper code (different number of line breaks, spaces etc., because of many edge cases)
    -- Solution to this problem is to interpret code line by line. However we have to guarantee
    -- that the code is formatted properly
    -- We cannot expect a user to always write such code. We can convert AST to list of operations (for a given ast, list of operations always remains the same)
    -- Also handling input/output becomes easier, since we don't have to use liftIO (because for an interpreter we would normally use a StateMonad)

    -- This should simplify pattern matching a bit (in the interpreter)
    data RegisterOP = STACK_PUSH String Exp 
                    | WHILE Exp
                    | STACK_MODIFY String Exp
                    | PRINT Exp -- STDOUT
                    | STACK_DELETE String
                    | IF Exp -- Internal IF that is used in conversion from DO LOOP to IF+JUMP mechanism (internally all loops are goto's statements)
                    | THEN
                    | ELSE
                    | ARMIF Exp -- Arithmetic IF
                    | NEGATIVEIF
                    | ZEROIF
                    | POSITIVEIF
                    | JUMP String
                    | LABEL String
                    | READ String -- STDIN (read to variable)
                    deriving Show 


    type Instructions = [(Int, RegisterOP)]

    -- Final code is a list of register ops
    generateCodeFromAST :: Com -> Instructions -> Int -> Instructions
    generateCodeFromAST (Assign name exp) acc line = acc ++ [(line, STACK_MODIFY name exp)]

    generateCodeFromAST (Print exp) acc line = acc ++ [(line, PRINT exp)]

    generateCodeFromAST (Read name) acc line = acc ++ [(line, READ name)]

    generateCodeFromAST (Jump label) acc line = acc ++ [(line, JUMP label)]

    generateCodeFromAST (Label label) acc line = acc ++ [(line, LABEL label)]

    generateCodeFromAST (Declare name exp com) acc line = acc ++ [(line, STACK_PUSH name exp)] ++ comArray ++ machineInstruction
        where comArray = generateCodeFromAST com [] (line+1)
              machineInstruction = [((length comArray) + line + 1, STACK_DELETE name)]

    generateCodeFromAST (ArmIf exp com1 com2 com3) acc line = acc ++ [(line, ARMIF exp), (line+1, NEGATIVEIF)] ++ negativeComArray ++ zeroIf ++ zeroComArray ++ positiveIf ++ positiveComArray
        where negativeComArray = generateCodeFromAST com1 [] (line+2)
              negativeArrayLength = length negativeComArray
              zeroIf = [(negativeArrayLength + line + 2, ZEROIF)]
              zeroComArray = generateCodeFromAST com2 [] (line + 3 + negativeArrayLength)
              zeroArrayLength = length zeroComArray
              positiveIf = [(negativeArrayLength + zeroArrayLength + line + 3, POSITIVEIF)]
              positiveComArray = generateCodeFromAST com1 [] (line + 4 + negativeArrayLength + zeroArrayLength)

    generateCodeFromAST (Seq com1 com2) acc line = acc ++ com1Array ++ com2Array
        where com1Array = generateCodeFromAST com1 [] line
              com2Array = generateCodeFromAST com2 [] (line + (length com1Array))

    generateCodeFromAST (While exp com) acc line = acc ++ [(line, WHILE exp)] ++ comArray
        where comArray = generateCodeFromAST com [] (line+1)

    generateCodeFromAST (DoLoop varName init end step com) acc line = acc ++ [(line, STACK_PUSH varName init)] ++ [(line+1, LABEL labelName)] ++ comArray ++ gotoMechanism -- Here is an example of swapping loop with goto (loop labels has special signature)
        where labelName = "loop_" ++ show (line+1)
              comArray = generateCodeFromAST com [] (line + 2)
              comArrayLength = length comArray
              offset = line + 2 + comArrayLength
              gotoMechanism = [(offset, IF (Less (Variable varName) end)), (offset+1, THEN), (offset+2, (STACK_MODIFY varName (Plus (Variable varName) step))), (offset+3, JUMP labelName), (offset+4, ELSE), (offset+5, STACK_DELETE varName)]

    astToInstructions :: Com -> Instructions
    astToInstructions ast = generateCodeFromAST ast [] 0

    -- testAST = (ArmIf (Constant 0) (Print (Constant 1)) (Seq (Print (Constant 1)) (Print (Constant 2137))) (Print (Constant 3)) )
    -- testAST2 = (ArmIf (Greater (Variable "x") (Constant 0)) (Print (Minus (Times (Constant 2) (Constant 4)) (Variable "x"))) (Print (Variable "x")) (Print (Variable "x")),"")
    -- testAST3 = Declare "x" (Constant 150) (Declare "y" (Constant 200) (Seq (While (Greater (Variable "x") (Constant 0)) (Seq (Assign "x" (Minus (Variable "x") (Constant 1))) (Assign "y" (Minus (Variable "y") (Constant 1))))) (Print (Variable "y"))))


    -- testAST4 = Declare "x" (Constant 150) (Declare "y" (Constant 200) (Seq (While (And (Or (Greater (Plus (Variable "x") (Constant 1)) (Constant 0)) (Greater (Minus (Variable "y") (Constant 10)) (Constant 0))) (Greater (Times (Variable "x") (Variable "x")) (Constant 20))) (Jump "y")) (Label "y")))

    -- testDoLoop = DoLoop "x" (Constant 10) (Constant 20) (Constant 100) (Print (Variable "x"))
    -- main :: IO ()
    -- main = do
    --     mapM_ putStrLn (map show (generateCodeFromAST testDoLoop [] 0))
    --     return ()