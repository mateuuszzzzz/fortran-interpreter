module PrettyPrinting where
    import Ast
    import Parser 
    import ParserCore 
    import Helpers (numberOfLines)

    -- Since this language has goto statements it is quite difficult to operate on the AST.
    -- Solution to this problem is to interpret code line by line. However we have to guarantee
    -- that the code is formatted properly
    -- We cannot expect a user to always write such code so we can convert AST back to String that is formatted properly.
    -- Such String can be easily interpreted line by line.
    -- Also handling input/output becomes easier, since we don't have to use liftIO (because for an interpreter we would normally use a StateMonad)

    -- This generates proper AST of our language 
    generateAST :: String -> Com
    generateAST code = fst $ head $ parse com code

    expToStr :: Exp -> String
    expToStr (Minus exp1 exp2) = "(" ++ expToStr exp1 ++ "-" ++ expToStr exp2 ++ ")"
    expToStr (Greater exp1 exp2) = "(" ++ expToStr exp1 ++ ">" ++ expToStr exp2 ++ ")" 
    expToStr (Less exp1 exp2) = "(" ++ expToStr exp1 ++ "<" ++ expToStr exp2 ++ ")"
    expToStr (Plus exp1 exp2) = "(" ++ expToStr exp1 ++ "+" ++ expToStr exp2 ++ ")"
    expToStr (Times exp1 exp2) = "(" ++ expToStr exp1 ++ "*" ++ expToStr exp2 ++ ")" 
    expToStr (Div exp1 exp2) = "(" ++ expToStr exp1 ++ "/" ++ expToStr exp2 ++ ")"   
    expToStr (Equal exp1 exp2) = "(" ++ expToStr exp1 ++ "==" ++ expToStr exp2 ++ ")" 
    expToStr (Constant n) = show n
    expToStr (Variable x) = x



    -- We don't care about proper tabulations since they will be ommited by interpreter
    -- This means that it's not the most pretty formatting, but it's useful
    generateCodeFromAST :: Com -> String -> Int -> String
    generateCodeFromAST (Assign name exp) acc line = acc ++ show line ++ ": " ++ name ++ " = " ++ expString ++  "\n"
        where expString = expToStr exp
    generateCodeFromAST (Print exp) acc line = acc ++ show line ++ ": print " ++ expString ++ "\n"
        where expString = expToStr exp
    generateCodeFromAST (Declare name exp com) acc line = acc ++ show line ++ ": " ++ "declare " ++ name ++ " = " ++ expString ++ "\n" ++ comString ++ machineInstruction
        where expString = expToStr exp
              comString = generateCodeFromAST com "" (line+1)
              machineInstruction = show ((numberOfLines comString) + line + 1) ++ ": STACK::DELETE " ++ name ++ "\n"
    generateCodeFromAST (ArmIf cond com1 com2 com3) acc line = acc ++ "IFF " ++ expToStr cond ++ "\nnegative\n" ++ generateCodeFromAST com1 "" 0 ++ "zero\n" ++ generateCodeFromAST com2 "" 0 ++ "positive\n" ++ generateCodeFromAST com3 "" 0
    generateCodeFromAST (Cond cond com1 com2) acc line = acc ++ "If " ++ expToStr cond ++ "\then\n" ++ generateCodeFromAST com1 "" 0 ++ "else\n" ++ generateCodeFromAST com2 "" 0
    generateCodeFromAST (Seq com1 com2) acc line = acc ++ com1String ++ com2String
        where com1String = generateCodeFromAST com1 "" line
              com2String = generateCodeFromAST com2 "" (line + (numberOfLines com1String))
    generateCodeFromAST (While exp com) acc line = acc ++ show line ++ ": while " ++ expString ++ " do\n" ++ comString
        where expString = expToStr exp
              comString = generateCodeFromAST com "" (line+1)



    testAST = (ArmIf (Constant 0) (Print (Constant 1)) (Seq (Print (Constant 1)) (Print (Constant 2137))) (Print (Constant 3)) )
    testAST2 = (ArmIf (Greater (Variable "x") (Constant 0)) (Print (Minus (Times (Constant 2) (Constant 4)) (Variable "x"))) (Print (Variable "x")) (Print (Variable "x")),"")
    testAST3 = Declare "x" (Constant 150) (Declare "y" (Constant 200) (Seq (While (Greater (Variable "x") (Constant 0)) (Seq (Assign "x" (Minus (Variable "x") (Constant 1))) (Assign "y" (Minus (Variable "y") (Constant 1))))) (Print (Variable "y"))))

    main :: IO ()
    main = do
        putStr (generateCodeFromAST testAST3 "" 0)
        return ()