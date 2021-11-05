module LLVM where

import Control.Monad.Except
import Control.Monad.State
import Data.Set as Set

import AbsInstant
import InstantError
import Operations

type NameEnv = Set.Set Ident

type MachineCode = [String]

data LLVMState = LLVMState {
    nameEnv :: NameEnv,
    nextReg :: Integer
} deriving Show

initLLVMState :: LLVMState
initLLVMState = LLVMState Set.empty 1

type LLVMMonad = StateT LLVMState (ExceptT CompilationError IO)

instance Show ArithmeticOp where
    show AddOp = "add"
    show SubOp = "sub"
    show MulOp = "mul"
    show DivOp = "sdiv"

getIndex :: LLVMMonad Integer
getIndex = do
    llvmState <- get
    put $ LLVMState (nameEnv llvmState) ((nextReg llvmState) + 1)
    return $ nextReg llvmState

data ExpResult
    = Number Integer
    | Evaluation Integer MachineCode
    deriving (Eq, Show)

compileOp :: ArithmeticOp -> Integer -> ExpResult -> ExpResult -> MachineCode
compileOp op outIdx (Number n) (Evaluation inIdx code) =
    code ++ [
        "  %" ++ (show outIdx) ++ " = " ++ (show op) ++ " i32 " ++ (show n) ++ ", %" ++ (show inIdx)
    ]

compileOp op outIdx (Evaluation inIdx code) (Number n) = 
    code ++ [
        "  %" ++ (show outIdx) ++ " = " ++ (show op) ++ " i32 %" ++ (show inIdx) ++ ", " ++ (show n) 
    ]

compileOp op outIdx (Evaluation in1 code1) (Evaluation in2 code2) =
    code1 ++ code2 ++ [
        "  %" ++ (show outIdx) ++ " = " ++ (show op) ++ " i32 %" ++ (show in1) ++ ", %" ++ (show in2)
    ]

compileOp op outIdx (Number n1) (Number n2) = [
        "  %" ++ (show outIdx) ++ " = " ++ (show op) ++ " i32 " ++ (show n1) ++ ", " ++ (show n2)
    ]

llvmStore :: String -> ExpResult -> MachineCode
llvmStore target value = case value of
    Evaluation source code -> 
        code ++ [
            "  store i32 %" ++ (show source) ++ ", i32* %" ++ target ++ ", align 4"
        ]
    Number n -> [
            "  store i32 " ++ (show n) ++ ", i32* %" ++ target ++ ", align 4"
        ]

llvmLoadIns :: Integer -> String -> String
llvmLoadIns target source = "  %" ++ (show target) ++ " = load i32, i32* %" ++ source

llvmLoad :: Integer -> String -> MachineCode
llvmLoad target source = [(llvmLoadIns target source)]

llvmAlloc :: String -> MachineCode
llvmAlloc var = [
        "  %" ++ var ++ " = alloca i32, align 4"
    ]

data PrintValueArg
    = Const Integer
    | Var String Integer
    | Reg Integer
    deriving Eq

printValue :: PrintValueArg -> MachineCode
printValue arg = case arg of
    Const c -> [
            "  call void @printInt(i32 " ++ (show c) ++ ")"
        ]
    Var name auxReg -> [
            (llvmLoadIns auxReg name),
            (head $ printValue (Reg auxReg))
        ]
    Reg index -> [
            "  call void @printInt(i32 %" ++ (show index) ++ ")"
        ]

-- Implementation of PrintInt:
--   https://moodle.mimuw.edu.pl/pluginfile.php/163444/mod_resource/content/1/runtime.ll
llvmPreamble :: MachineCode
llvmPreamble = [
        "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
        "declare i32 @printf(i8*,...)",
        "define void @printInt(i32 %x) {",
        "       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
        "       call i32 (i8*, ...) @printf(i8* %t0, i32 %x)",
        "       ret void",
        "}",
        "",
        "define i32 @main() {"
    ]

llvmMainEnd :: MachineCode
llvmMainEnd = [
        "  ret i32 0",
        "}"
    ]