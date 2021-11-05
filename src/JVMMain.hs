module Main where

import System.Environment
import System.Exit
import System.Process
import System.FilePath

import AbsInstant
import ErrM
import ParInstant

import InstantError
import JVM
import JVMCompiler

exitWithErrorMsg :: String -> IO ()
exitWithErrorMsg msg = do
    putStrLn msg
    exitFailure

jasminCmd :: String
jasminCmd = "java -jar lib/jasmin.jar -d "

runFile :: FilePath -> IO ()
runFile inputPath = readFile inputPath >>= run inputPath

run :: String -> FilePath -> IO ()
run instantCode outputPath =
    case pProgram (myLexer instantCode) of
        Bad err -> exitWithErrorMsg $ "Parse error." ++ err
        Ok tree -> do
            jvmCode <- runJVMCompiler tree
            case jvmCode of
                Left err -> exitWithErrorMsg $ show err
                Right (result, jvmState) -> do
                    writeFile (replaceExtension outputPath "j") $ unlines $ (jvmPreamble (takeBaseName outputPath)) ++ 
                        (jvmMainPreamble (nextVar jvmState) (maxStackSize jvmState)) ++ result ++ jvmMainEnd
                    child <- runCommand $ jasminCmd ++ (takeDirectory outputPath) ++ 
                        " " ++ (replaceExtension outputPath "j")
                    waitForProcess child
                    exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [] -> putStrLn "Input file name"
        fs -> mapM_ runFile fs
