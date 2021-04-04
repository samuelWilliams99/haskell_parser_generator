module ParserGenerator (runParserGenerator, generateParser, pathToModule, module Result) where

import System.IO
import System.Directory
import System.Environment
import System.FilePath.Posix
import Result
import Scanner
import Cfgparser
import Grammar
import ShiftReduce
import DFA
import ParserCodeGenerator
import Data.HashMap.Strict as Map
import Data.Char

-- Run with first arg as path
main :: IO ()
main = do
    args <- getArgs
    runParserGenerator $ head args

-- Search for file, check path, generate and write new file
runParserGenerator :: String -> IO ()
runParserGenerator path
    | takeExtension path /= ".gmr" = putStrLn "Invalid file extension, must be .gmr"
    | otherwise = do
        exists <- doesFileExist path
        if not exists then
            putStrLn ("No such file \"" ++ path ++ "\"")
        else do
            contents <- readFile path
            case generateParser contents $ pathToModule path of
                Error e -> putStrLn e
                Result code -> do
                    writeFile outPath code
                  where
                    outPath = replaceExtension path "hs"

-- Converts a full path to a module name : my/path/to/parser.hs -> Parser
pathToModule :: String -> String
pathToModule path = (toUpper $ head name):(tail name)
  where
    name = takeBaseName path

-- Formalises the list of scanner spec pairs into one structure, validating
generateScannerSpec :: [(String, [String])] -> Result ScannerSpec
generateScannerSpec raw = generateScannerSpecAux raw scannerSpec
  where
    generateScannerSpecAux [] spec = return spec
    generateScannerSpecAux (x:xs) spec = do
        spec' <- generateScannerSpecAux xs spec
        case x of
            ("ops", ops) -> return $ spec'{ specOperators=(specOperators spec') ++ ops }
            ("kwds", kwds) -> return $ spec'{ specKeywords=(specKeywords spec') ++ kwds }
            ("sepiden", _) -> return $ spec'{ specSeparateCasedIdentifiers=True }
            ("keepspace", _) -> return $ spec'{ specIgnoreWhitespace=False }
            ("keepcmts", _) -> return $ spec'{ specIgnoreComments=False }
            ("line", (line:_)) -> case specLineComment spec' of
                Nothing   -> return $ spec'{ specLineComment=Just line }
                otherwise -> Error "Multiple line comment definitions"
            ("block", (open:close:_)) -> case specBlockComment spec' of
                Nothing   -> return $ spec'{ specBlockComment=Just (open, close) }
                otherwise -> Error "Multiple block comment definitions"
            ("parser", (code:_)) -> case specParserMap spec' of
                Nothing   -> return $ spec'{ specParserMap=Just code }
                otherwise -> Error "Multiple extra parser definitions"

-- Parse, build scanner spec and DFA, then generate code.
generateParser :: String -> String -> Result String
generateParser str name = do
    (exports, preCode, scannerSpecRaw, grammar) <- runParser str
    scannerSpec <- generateScannerSpec scannerSpecRaw

    dfa <- generateDFA $ addScannerSpecTokens scannerSpec grammar

    return $ generateCode name exports preCode scannerSpec dfa
