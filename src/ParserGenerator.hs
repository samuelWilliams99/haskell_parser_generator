{-|
Module      : ParserGenerator
Description : Generates Haskell Parsers from Context Free Grammar definitions, via a .gmr file.
Copyright   : (c) Samuel Williams, 2021
License     : GPL-3
Maintainer  : samuel.will1999@gmail.com
Stability   : release

This module takes a .gmr file (or the contents of such a file in String form) and outputs the code for a Haskell parser defined by that file.
This can be called in an @Either String String@ monad, where @Right@ indicates success and gives the output code, and @Left@ indicates an error, and gives a message.
Normally however, this will be called using the @IO@ function, handling file input and output itself.
The specification of the .gmr file can be found here: <https://github.com/samuelWilliams99/haskell_parser_generator#gmr-format>
-}
module ParserGenerator (runParserGenerator, generateParser, pathToModule, parserRequirements) where

import System.IO
import System.Directory
import System.Environment
import System.FilePath.Posix
import ParserGenerator.Cfgparser
import ParserGenerator.Grammar
import ParserGenerator.ShiftReduce
import ParserGenerator.DFA
import ParserGenerator.ParserCodeGenerator
import ParserGenerator.ParserRequirementsCode
import ParserGenerator.ParserRequirements
import Data.HashMap.Strict as Map
import Data.Char
import Data.Maybe

-- Run with first arg as path
main :: IO ()
main = do
    args <- getArgs
    runParserGenerator $ head args

-- Search for file, check path, generate and write new file
-- | Takes the path to an input .gmr file, ensures it is correct, then generates the associated .hs file containing the Parser.
-- On failure, the IO will print to console containing ther error, and stop.
runParserGenerator :: String -> IO ()
runParserGenerator path
    | takeExtension path /= ".gmr" = putStrLn "Invalid file extension, must be .gmr"
    | otherwise = do
        exists <- doesFileExist path
        if not exists then
            putStrLn ("No such file \"" ++ path ++ "\"")
        else do
            contents <- readFile path
            case generateParser' contents (pathToModule path) id of
                Error e -> putStrLn e
                Result code -> do
                    writeFile codeOutPath code
                    writeFile reqsOutPath parserRequirements
                  where
                    codeOutPath = replaceExtension path "hs"
                    reqsOutPath = replaceFileName path "parserrequirements.hs"

-- | Converts a full path to a module name, for example:
-- @my/path/to/parser.hs@ -> @Parser@
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
            ("parser", (code:codeType:_)) -> case specParserMap spec' of
                Nothing   -> return $ spec'{ specParserMap=Just (code, codeType) }
                otherwise -> Error "Multiple extra parser definitions"

-- Parse, build scanner spec and DFA, then generate code.
generateParser' :: String -> String -> (Maybe String -> Maybe String) -> Result String
generateParser' str name exportsMap = do
    (exports, preCode, scannerSpecRaw, grammar) <- runParser str
    scannerSpec <- generateScannerSpec scannerSpecRaw

    dfa <- generateDFA $ addScannerSpecTokens scannerSpec grammar

    return $ generateCode name (exportsMap exports) preCode scannerSpec dfa

-- | Generates a parser without the use of the IO monad, for when you wish to handle IO yourself
generateParser :: String -- ^ The input gmr definition
               -> String -- ^ The module name for the outputted file
               -> (Maybe String -> Maybe String) -- ^ A map function applied to the exports, in case functions defined in @%precode@ need to be exported
               -> Either String String -- ^ The output code or error, @Right@ indicates success, @Left@ indicates failure
generateParser str name exportsMap = case generateParser' str name exportsMap of
    Error e -> Left e
    Result c -> Right c
