module JVMCompiler where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

import AbsInstant
import InstantError
import JVM
import Operations

evalArithmeticOp :: ArithmeticOp -> Exp -> Exp -> JVMMonad (MachineCode, Integer)
evalArithmeticOp op exp1 exp2 = do
    (exp1JVM, exp1Stack) <- evalExp exp1
    (exp2JVM, exp2Stack) <- evalExp exp2
    if getCommutativity op == NonCommOp || exp1Stack > exp2Stack
        then return (compileOp op exp1JVM exp2JVM, computeStack op exp1Stack exp2Stack)
        else return (compileOp op exp2JVM exp1JVM, computeStack op exp2Stack exp1Stack)


evalExp :: Exp -> JVMMonad (MachineCode, Integer)
evalExp exp = case exp of
    ExpAdd term1 term2 -> evalArithmeticOp AddOp term1 term2
    ExpSub term1 term2 -> evalArithmeticOp SubOp term1 term2
    ExpMul factor1 factor2 -> evalArithmeticOp MulOp factor1 factor2
    ExpDiv dividend divisor -> evalArithmeticOp DivOp dividend divisor
    ExpLit value -> return (pushInteger value, 1)
    ExpVar ident -> do
        jvmState <- get
        case Map.lookup ident (varEnv jvmState) of
            Just i -> return ((compileLocalVarOp JVMLoad i), 1)
            Nothing -> throwError $ UndeclaredVar ident

evalStmt :: Stmt -> JVMMonad MachineCode
evalStmt (SExp exp) = do
    jvmState <- get
    (code, stmtStack) <- evalExp exp
    put $ JVMState (max (maxStackSize jvmState) (stmtStack + 1)) (varEnv jvmState) (nextVar jvmState)
    return $ printValue code

evalStmt (SAss ident exp) = do
    (code, expStack) <- evalExp exp
    jvmState <- get
    put $ JVMState (max expStack (maxStackSize jvmState)) (varEnv jvmState) (nextVar jvmState) 
    updatedState <- get
    case Map.lookup ident (varEnv updatedState) of
        Just index -> return $ code ++ (compileLocalVarOp JVMStore index)
        Nothing -> do
            put $ JVMState (maxStackSize updatedState) (Map.insert ident (nextVar updatedState) (varEnv updatedState)) ((nextVar updatedState) + 1)
            newVarState <- get
            return $ code ++ (compileLocalVarOp JVMStore ((varEnv newVarState) Map.! ident))

evalStmts :: [Stmt] -> [MachineCode] -> JVMMonad MachineCode
evalStmts (stmt:stmts) jvmIns = do
    jvmStmt <- evalStmt stmt
    evalStmts stmts (jvmIns ++ [jvmStmt])
evalStmts [] jvmIns = return $ concat jvmIns 

evalProgram :: Program -> JVMMonad MachineCode
evalProgram (Prog ss) = evalStmts ss []

compileJVM :: Program -> ExceptT CompilationError IO (MachineCode, JVMState)
compileJVM prog = runStateT (evalProgram prog) (initJVMState)

runJVMCompiler :: Program -> IO (Either CompilationError (MachineCode, JVMState))
runJVMCompiler prog = runExceptT $ compileJVM prog