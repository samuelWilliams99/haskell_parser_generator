module ParserCodeGenerator (generateCode) where

import DFA
import Grammar
import ShiftReduce
import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split
import Data.HashMap.Strict as Map
import Control.Lens

-- Generate each section of code and concat together
generateCode :: String -> String -> ScannerSpec -> DFA -> String
generateCode name preCode scannerSpec (DFA ss' ps tm _ _) =
    concat [ generateModuleDef name, "\n\n"
           , imports, "\n"
           , trim preCode, "\n\n"
           , generateScannerCode scannerSpec, "\n\n"
           , startCode, "\n\n"
           , generateStatesList $ length ss', "\n\n"
           , generateAbsSynDataType ps, "\n\n"
           , generateStatesCode ss' ps tm
           , generateReductions ps tm]

generateModuleDef :: String -> String
generateModuleDef name = "module " ++ name ++ " (runParser, module Result) where"

imports :: String
imports = "import Scanner\nimport Parsing\nimport Result\nimport ParseState"

trim = dropWhileEnd isSpace . dropWhile isSpace
replace old new = intercalate new . splitOn old

-- Build code to create a Scanner from the ScannerSpec
generateScannerCode :: ScannerSpec -> String
generateScannerCode spec = "gScanner = Scanner{ separateCasedIdentifiers=" ++ (show $ specSeparateCasedIdentifiers spec) ++
                         "\n                  , ignoreWhitespace=" ++ (show $ specIgnoreWhitespace spec) ++
                         "\n                  , ignoreComments=" ++ (show $ specIgnoreComments spec) ++
                         "\n                  , operators=" ++ (show $ specOperators spec) ++
                         "\n                  , keywords=" ++ (show $ specKeywords spec) ++
                         "\n                  , lineComment=" ++ (show $ specLineComment spec) ++
                         "\n                  , blockComment=" ++ (show $ specBlockComment spec) ++
                         "\n                  , includeEOF=True" ++
                         "\n                  , parserMap=" ++ (trim $ fromMaybe "id" $ specParserMap spec) ++
                         "\n                  }"

-- Defines entrypoint, error point, and util unpack function
startCode :: String
startCode =
    "runParser str = do\n\
    \    ts <- scan gScanner str\n\
    \    generatedState0 [] [] $ fmap AbsSynToken ts\n\n\
    \generatedError [] = Error \"Ran out of tokens\"\n\
    \generatedError ((AbsSynToken (Token ps x)):xs) = Error $ \"Unexpected token: \" ++ (show x) ++ \" at \" ++ showPos ps\n\n\
    \unpackFinal (AbsSynResult1 x) = x"

-- List of state functions
generateStatesList :: Int -> String
generateStatesList n = "generatedStates = [" ++ intercalate ", " ["generatedState" ++ (show n') | n' <- [0..n - 1]] ++ "]"

-- Create the state output data type
generateAbsSynDataType :: [DFAProduction] -> String
generateAbsSynDataType ps = "data AbsSynToken" ++ (concat $ fmap (" t"++) absSynNs) ++ " = AbsSynToken Token" ++
    (concat $ fmap (\x -> " | AbsSynResult" ++ x ++ " t" ++ x) absSynNs)
  where
    nonTerminalCount = length $ getNonTerminals ps
    absSynNs = fmap show [1..nonTerminalCount-1]

-- Call and concat generateStateCode on each state, retaining index
generateStatesCode :: [DFAState] -> [DFAProduction] -> TokenMap -> String
generateStatesCode ss' ps tm = aux ss' 0
  where
    aux [] n = ""
    aux (s:ss) n = generateStateCode s n ps tm ++ "\n\n\n" ++ (aux ss $ n + 1)

-- Generate each state function
generateStateCode :: DFAState -> Int -> [DFAProduction] -> TokenMap -> String
generateStateCode (DFAState _ as' ) n ps tm = aux as'
  where
    -- generatedStateX :: [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result AbsSynToken
    aux [] = "generatedState" ++ (show n) ++ " vs ss xs = generatedError xs"
    aux ((t, a):as) = "generatedState" ++ (show n) ++ " vs ss (x@" ++ generateStatePattern ps tm t ++ ":xs) = "
        ++ generateStateAction n a ps ++ "\n"
        ++ aux as

-- Generate the pattern for matching next tokens
generateStatePattern :: [DFAProduction] -> TokenMap -> RuleTokenType -> String
generateStatePattern ps tm (RuleTerminal t) = "(AbsSynToken (Token ps (" ++ (replace "$$" "_" $ trim $ tokenPattern $ tm ! t) ++ ")))"
generateStatePattern ps tm (RuleNonTerminal t) = "(AbsSynResult" ++ (show $ getProdIndex t ps) ++ " _)"

-- Generate the action code for shifting, reducing or finishing
generateStateAction :: Int -> DFAAction -> [DFAProduction] -> String
generateStateAction i DFAFinish ps = "return $ unpackFinal $ head vs"
generateStateAction i (DFAShift i') ps = "generatedState" ++ (show i') ++ " (x:vs) (" ++ (show i) ++ ":ss) xs"
generateStateAction i (DFAReduce i') ps = nextStateStr ++ " " ++ dropStrVs ++ " " ++ dropStr ++ " (x':x:xs)\n\
\  where x' = generatedReduction" ++ show i' ++ " vs"
  where
    prod = ps !! i'
    result = prod^.dfaProductionResult
    len = prod^.dfaProductionTokens.to length
    dropStrVs = "(drop " ++ show len ++ " vs)"
    dropStr = "(drop " ++ show len ++ " ss)"
    nextStateStr = if len == 0 then "generatedState" ++ show i
        else "(generatedStates !! (ss !! " ++ (show $ len - 1) ++ "))"

getProdIndex :: String -> [DFAProduction] -> Int
getProdIndex s ps = fromJust $ findIndex (==s) $ getNonTerminals ps

-- Call and concat generateReductions on each production, retaining index
generateReductions :: [DFAProduction] -> TokenMap -> String
generateReductions ps tm = concat [(generateReduction v ps tm) ++ "\n\n" | v <- [1..length ps - 1] ]

-- Pattern match expected tokens for reduction, call result code from gmr file, pack result in correct AbsSynToken constructor
generateReduction :: Int -> [DFAProduction] -> TokenMap -> String
generateReduction i ps tm = "generatedReduction" ++ (show i) ++ " " ++ vPattern ++
    " = AbsSynResult" ++ (show $ getProdIndex (p^.dfaProductionName) ps) ++
    " (" ++ (p^.dfaProductionResult.to trim) ++ ")"
  where
    p = ps !! i
    prodTokens = p^.dfaProductionTokens
    vPattern = "(" ++ (concat $ reverse [viPattern (show $ i + 1) (prodTokens !! i) ++ ":" | i <- [0..length prodTokens - 1]]) ++ "_)"
    viPattern i' (RuleTerminal t) = concat ["(AbsSynToken (Token ps", i', " ", terminalPattern t i', "))"]
    viPattern i' (RuleNonTerminal prod) = concat ["(AbsSynResult", show $ getProdIndex prod ps, " v", i', ")"]
    terminalPattern t i' = if length patternSplit == 1 then v else "(" ++ intercalate v patternSplit ++ ")"
      where
        v = "v" ++ i'
        patternSplit = splitOn "$$" $ trim $ tokenPattern $ tm ! t