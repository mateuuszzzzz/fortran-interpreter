module PrettyPrinting (RegisterOP (STACK_PUSH, STACK_MODIFY, PRINT, STACK_DELETE, IF, THEN, ELSE, ARMIF, NEGATIVEIF, ZEROIF, POSITIVEIF, JUMP, LABEL, READ, ENDPROG), Instructions, Instruction, generateCodeFromAST, astToInstructions) where
    import Ast
    
    -- Since this language has goto statements it is quite difficult to operate on the AST.
    -- Also, it would be difficult to create an interpreter that accepts any proper code (different number of line breaks, spaces etc., because of many edge cases)
    -- Solution to this problem is to interpret code line by line. However we have to guarantee
    -- that the code is formatted properly
    -- We cannot expect a user to always write such code. We can convert AST to list of operations (for a given ast, list of operations always remains the same)
    -- Also handling input/output becomes easier, since we don't have to use liftIO (because for an interpreter we would normally use a StateMonad)

    -- This should simplify pattern matching a bit (in the interpreter)
    data RegisterOP = STACK_PUSH String Exp -- OK
                    | STACK_MODIFY String Exp -- OK
                    | PRINT Exp -- STDOUT -- OK 
                    | STACK_DELETE String -- OK
                    | IF Exp Int Int -- OK Internal IF that is used in conversion from DO LOOP to IF+JUMP mechanism (internally all loops are goto's statements)
                    | THEN -- OK
                    | ELSE Int -- OK
                    | ARMIF Exp Int Int Int -- OK Arithmetic IF (Those additional ints are like pointers to the right place in the list of registers. Without it we don't know which block is next, because we may have nested ifs. Same goes for internal IF instruction)
                    | NEGATIVEIF -- OK
                    | ZEROIF Int -- OK
                    | POSITIVEIF Int -- OK
                    | JUMP String -- OK
                    | LABEL String -- OK
                    | READ String -- STDIN (read to variable)
                    | ENDPROG -- OK End of program
                    deriving Show 


    type Instruction = (Int, RegisterOP)
    type Instructions = [Instruction]

    -- Final code is a list of register ops
    generateCodeFromAST :: Stmt -> Instructions -> Int -> Instructions
    generateCodeFromAST (Assign name exp) acc line = acc ++ [(line, STACK_MODIFY name exp)]

    generateCodeFromAST (Print exp) acc line = acc ++ [(line, PRINT exp)]

    generateCodeFromAST (Read name) acc line = acc ++ [(line, READ name)]

    generateCodeFromAST (Jump label) acc line = acc ++ [(line, JUMP label)]

    generateCodeFromAST (Label label) acc line = acc ++ [(line, LABEL label)]

    generateCodeFromAST (Float name exp com) acc line = acc ++ [(line, STACK_PUSH name exp)] ++ comArray ++ machineInstruction
        where comArray = generateCodeFromAST com [] (line+1)
              machineInstruction = [((length comArray) + line + 1, STACK_DELETE name)]

    generateCodeFromAST (ArmIf exp com1 com2 com3) acc line = acc ++ [(line, ARMIF exp (line+2) offset1 (offset2+1)), (line+1, NEGATIVEIF)] ++ negativeComArray ++ zeroIf ++ zeroComArray ++ positiveIf ++ positiveComArray
        where negativeComArray = generateCodeFromAST com1 [] (line+2) -- neg stmts
              zeroComArray = generateCodeFromAST com2 [] (line + 3 + negativeArrayLength) -- zero stmts
              positiveComArray = generateCodeFromAST com3 [] (line + 4 + negativeArrayLength + zeroArrayLength) -- pos stmts
              negativeArrayLength = length negativeComArray
              zeroArrayLength = length zeroComArray
              positiveArratLength = length positiveComArray
              offset1 = line + negativeArrayLength + 3
              offset2 = offset1 + zeroArrayLength
              offset3 = offset2 + positiveArratLength
              zeroIf = [(negativeArrayLength + line + 2, ZEROIF (offset3+1) )]
              positiveIf = [(offset2, POSITIVEIF(offset3+1))]

    generateCodeFromAST (Seq com1 com2) acc line = acc ++ com1Array ++ com2Array
        where com1Array = generateCodeFromAST com1 [] line
              com2Array = generateCodeFromAST com2 [] (line + (length com1Array))

    generateCodeFromAST (DoLoop varName init end step com) acc line = acc ++ [(line, STACK_PUSH varName init)] ++ initGotoMechanism ++ [(line+6, LABEL labelName)] ++ comArray ++ gotoMechanism -- Here is an example of swapping loop with goto (loop labels has special signature)
        where labelName = "loop_" ++ show (line+1)
              loopEndLabel = labelName ++ "_end"
              initGotoMechanism = [(line+1, IF (Less (Variable varName) end) (line+3) (line+5)), (line+2, THEN), (line+3, JUMP labelName), (line+4, ELSE (line+6)),  (line+5, JUMP loopEndLabel)] -- This checks condition at the beginning of the loop
              offset1 = length initGotoMechanism
              comArray = generateCodeFromAST com [] (line + 2 + offset1)
              comArrayLength = length comArray
              offset2 = line + 2 + comArrayLength + offset1 -- Last gotoMechanism checks condition after incrementation (after execution of the loop)
              gotoMechanism = [(offset2, (STACK_MODIFY varName (Plus (Variable varName) step))),(offset2+1, IF (Less (Variable varName) end) (offset2 + 3) (offset2 + 5)), (offset2+2, THEN), (offset2+3, JUMP labelName), (offset2+4, ELSE (offset2+7)), (offset2+5, LABEL loopEndLabel), (offset2+6, STACK_DELETE varName)]

    astToInstructions :: Stmt -> Instructions
    astToInstructions ast = prog ++ progEnd 
        where prog = (generateCodeFromAST ast [] 0)
              progEnd = [(length prog, ENDPROG)]

    -- testAST = (ArmIf (Constant 0) (Print (Constant 1)) (Seq (Print (Constant 1)) (Print (Constant 2137))) (Print (Constant 3)) )
    -- -- -- testAST2 = (ArmIf (Greater (Variable "x") (Constant 0)) (Print (Minus (Times (Constant 2) (Constant 4)) (Variable "x"))) (Print (Variable "x")) (Print (Variable "x")),"")
    -- -- -- testAST3 = Declare "x" (Constant 150) (Declare "y" (Constant 200) (Seq (While (Greater (Variable "x") (Constant 0)) (Seq (Assign "x" (Minus (Variable "x") (Constant 1))) (Assign "y" (Minus (Variable "y") (Constant 1))))) (Print (Variable "y"))))


    -- -- -- testAST4 = Declare "x" (Constant 150) (Declare "y" (Constant 200) (Seq (While (And (Or (Greater (Plus (Variable "x") (Constant 1)) (Constant 0)) (Greater (Minus (Variable "y") (Constant 10)) (Constant 0))) (Greater (Times (Variable "x") (Variable "x")) (Constant 20))) (Jump "y")) (Label "y")))

    -- testDoLoop = DoLoop "x" (Constant 10.0) (Constant 20.0) (Constant 100.0) (Print (Variable "x"))
    -- main :: IO ()
    -- main = do
    --      mapM_ putStrLn (map show (astToInstructions testAST))
    --      return ()