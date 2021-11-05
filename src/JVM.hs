module JVM where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

import AbsInstant
import InstantError
import Operations

type VarEnv = Map.Map Ident Integer

type MachineCode = [String]

data JVMState = JVMState {
    maxStackSize :: Integer,
    varEnv :: VarEnv,
    nextVar :: Integer
} deriving Show

initJVMState :: JVMState
initJVMState = JVMState 0 Map.empty 1

type JVMMonad = StateT JVMState (ExceptT CompilationError IO)

computeStack :: ArithmeticOp -> Integer -> Integer -> Integer
computeStack op size1 size2 = case getCommutativity op of
    NonCommOp -> max size1 (size2 + 1)
    CommOp -> if size1 == size2 
        then size1 + 1
        else max size1 size2

pushInteger :: Integer -> MachineCode
pushInteger value
    | value < 6 = ["  iconst_" ++ (show value)]
    | otherwise = ["  bipush " ++ (show value)]

instance Show ArithmeticOp where
    show AddOp = "iadd"
    show SubOp = "isub"
    show MulOp = "imul"
    show DivOp = "idiv"

compileOp :: ArithmeticOp -> MachineCode -> MachineCode -> MachineCode
compileOp op exp1 exp2 = exp1 ++ exp2 ++ ["  " ++ (show op)]

printValue :: MachineCode -> MachineCode
printValue code = [
        "  getstatic",
        "    java/lang/System/out Ljava/io/PrintStream;"
    ] ++ code ++ [
        "  invokevirtual",
        "    java/io/PrintStream/println(I)V"
    ]

data LocalVarOp = JVMLoad | JVMStore
instance Show LocalVarOp where
    show JVMLoad = "iload"
    show JVMStore = "istore"

localVarOpForm :: Integer -> String
localVarOpForm i = (getInfix i) ++ (show i)
    where
        getInfix :: Integer -> String
        getInfix i = if i < 4 
            then "_" 
            else " "

compileLocalVarOp :: LocalVarOp -> Integer -> MachineCode
compileLocalVarOp op i = [" " ++ (show op) ++ (localVarOpForm i)]

jvmPreamble :: String -> MachineCode
jvmPreamble className = [
        ".class public " ++ className,
        ".super java/lang/Object",
        ".method public <init>()V",
        "  aload_0",
        "  invokespecial java/lang/Object/<init>()V",
        "  return",
        ".end method",
        ""
    ]

jvmMainPreamble :: Integer -> Integer -> MachineCode
jvmMainPreamble localsSize stackSize = [
        ".method public static main([Ljava/lang/String;)V",
        "  .limit locals " ++ (show localsSize),
        "  .limit stack " ++ (show stackSize)
    ]

jvmMainEnd :: MachineCode
jvmMainEnd = [
        "  return",
        ".end method"
    ]

jvmMain :: Integer -> Integer -> MachineCode -> MachineCode
jvmMain localsSize stackSize body = (jvmMainPreamble localsSize stackSize) ++ body ++ jvmMainEnd
