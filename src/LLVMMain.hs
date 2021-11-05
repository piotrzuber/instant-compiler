module Main where

import System.Environment
import System.Exit
import System.Process
import System.FilePath

import AbsInstant
import ErrM
import ParInstant

import InstantError
import LLVM
import LLVMCompiler

exitWithErrorMsg :: String -> IO ()
exitWithErrorMsg msg = do
    putStrLn msg
    exitFailure

runFile :: FilePath -> IO ()
runFile inputPath = readFile inputPath >>= run inputPath

run :: FilePath -> String -> IO ()
run outputPath instantCode =
    case pProgram (myLexer instantCode) of
        Bad err -> exitWithErrorMsg $ "Parse error." ++ err
        Ok tree -> do
            llvmCode <- runLLVMCompiler tree
            case llvmCode of
                Left err -> exitWithErrorMsg $ show err
                Right (result, jvmState) -> do
                    writeFile (replaceExtension outputPath "ll") $ unlines $ llvmPreamble ++ result ++ llvmMainEnd
                    child <- runCommand $ "llvm-as " ++ (replaceExtension outputPath "ll")
                    waitForProcess child
                    exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [] -> putStrLn "Input file name"
        fs -> mapM_ runFile fs
