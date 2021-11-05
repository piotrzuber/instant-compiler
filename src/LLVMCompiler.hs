module LLVMCompiler where

import Control.Monad.Except
import Control.Monad.State
import Data.Set as Set

import AbsInstant
import InstantError
import LLVM
import Operations

evalArithmeticOp :: ArithmeticOp -> Exp -> Exp -> LLVMMonad ExpResult
evalArithmeticOp op exp1 exp2 = do
    exp1Result <- evalExp exp1
    exp2Result <- evalExp exp2
    index <- getIndex
    return $ Evaluation index (compileOp op index exp1Result exp2Result)

evalExp :: Exp -> LLVMMonad ExpResult
evalExp e = case e of
    ExpAdd term1 term2 -> evalArithmeticOp AddOp term1 term2
    ExpSub term1 term2 -> evalArithmeticOp SubOp term1 term2
    ExpMul factor1 factor2 -> evalArithmeticOp MulOp factor1 factor2
    ExpDiv dividend divisor -> evalArithmeticOp DivOp dividend divisor
    ExpLit value -> return $ Number value
    ExpVar (Ident ident) -> do
        llvmState <- get
        index <- getIndex
        if Set.member (Ident ident) (nameEnv llvmState)
            then return $ Evaluation index (llvmLoad index ident)
            else throwError $ UndeclaredVar (Ident ident)

evalStmt :: Stmt -> LLVMMonad MachineCode
evalStmt (SExp exp) = case exp of
    ExpLit value -> return $ printValue $ Const value
    ExpVar (Ident ident) -> do
        index <- getIndex
        return $ printValue $ Var ident index
    _ -> do
        (Evaluation index code) <- evalExp exp
        return $ code ++ (printValue $ Reg index)

evalStmt ass@(SAss id@(Ident ident) exp) = do
    llvmState <- get
    if Set.member id (nameEnv llvmState)
        then do
            expResult <- evalExp exp
            return $ llvmStore ident expResult
        else do
            put $ LLVMState (Set.insert id (nameEnv llvmState)) (nextReg llvmState)
            knownIdentCode <- evalStmt ass
            return $ (llvmAlloc ident) ++ knownIdentCode

evalStmts :: [Stmt] -> [MachineCode] -> LLVMMonad MachineCode
evalStmts (stmt:stmts) llvmIns = do
    llvmStmt <- evalStmt stmt
    evalStmts stmts (llvmIns ++ [llvmStmt])
evalStmts [] llvmIns = return $ concat llvmIns 

evalProgram :: Program -> LLVMMonad MachineCode
evalProgram (Prog ss) = evalStmts ss []

compileLLVM :: Program -> ExceptT CompilationError IO (MachineCode, LLVMState)
compileLLVM prog = runStateT (evalProgram prog) (initLLVMState)

runLLVMCompiler :: Program -> IO (Either CompilationError (MachineCode, LLVMState))
runLLVMCompiler prog = runExceptT $ compileLLVM prog