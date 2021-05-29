{-|
Module      : ParserCodeGenerator
Description : Takes information about the @Scanner@ and @DFA@ and outputs haskell code
Copyright   : (c) Samuel Williams, 2021
License     : GPL-3
Maintainer  : samuel.will1999@gmail.com
Stability   : release

Code generator for the parser, all validity checks have already passed at this point, thus the single function in this module cannot fail
-}
module ParserCodeGenerator (generateCode) where

import DFA
import Grammar
import ShiftReduce
import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split
import Data.HashMap.Strict as Map hiding (map, filter)
import Control.Lens
import Data.Function

reindent :: Int -> String -> String
reindent n str = intercalate "\n" $ map (\line -> (replicate n ' ') ++ (drop minIndent line)) strLines
  where
    minIndent = minimum $ map (length . fst . span isSpace) $ strLines
    strLines = filter ((>0) . length . trim) $ lines str

unindent :: String -> String
unindent = reindent 0

-- | Takes @Scanner@ and @DFA@ information and output the parser code
generateCode :: String -- ^ The generated modules name
             -> Maybe String -- ^ Optional additional exports
             -> String -- ^ Initial haskell code in output
             -> ScannerSpec -- ^ @ScannerSpec@ as defined in "Grammar"
             -> DFA -- ^ @DFA@ as defined in "DFA"
             -> String -- ^ Outputted parser haskell code
generateCode name exports preCode scannerSpec (DFA ss ps tm _ fm) =
    concat [ generateModuleDef name exports, "\n\n"
           , imports, "\n"
           , unindent preCode, "\n\n"
           , generateScannerCode scannerSpec, "\n\n"
           , startCode, "\n\n"
           , generateStatesList $ length ss, "\n\n"
           , dataType, "\n\n"
           , generateStatesCode ss ps tm isDefined
           , generateReductions ps tm isDefined]
  where
    (dataType, isDefined) = generateAbsSynDataType ps $ snd $ fromMaybe ("", "()") $ specParserMap scannerSpec

generateModuleDef :: String -> Maybe String -> String
generateModuleDef name mExports = unlines [
    "{-|",
    "Module      : " ++ name,
    "Description : Parser generated with the Haskell Parser Generator - https://github.com/samuelWilliams99/haskell_parser_generator",
    "-}",
    "module " ++ name ++ " (runParser, module ParserRequirements" ++ exports ++ ") where"
    ]
  where
    exports = case mExports of
                  Nothing -> ""
                  Just e  -> ", " ++ trim e

imports :: String
imports = "import ParserRequirements\nimport Control.Applicative"

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
                         "\n                  , parserMap=" ++ (trim $ fst $ fromMaybe ("id", "") $ specParserMap spec) ++
                         "\n                  }"

-- Defines entrypoint, error point, and util unpack function
startCode :: String
startCode = unlines [
    "-- | Generates the Abstract Syntax Tree from an input string, can fail",
    "runParser str = do",
    "    ts <- scan gScanner str",
    "    let ps = if length ts == 0 then parseState \"\" else let (Token ps' _) = head ts in ps'",
    "    generatedState0 ps [] [] $ fmap AbsSynToken ts",
    "",
    "generatedError n [] = Error \"Ran out of tokens\"",
    "generatedError n ((AbsSynToken (Token ps x)):xs) = Error $ \"Unexpected token: \" ++ (show x) ++ \" at \" ++ showPos ps",
    "",
    "unpackFinal (AbsSynResult1 x _) = x"
    ]

-- List of state functions
generateStatesList :: Int -> String
generateStatesList n = "generatedStates = [" ++ intercalate ", " ["generatedState" ++ (show n') | n' <- [0..n - 1]] ++ "]"

-- Create the state output data type
generateAbsSynDataType :: [DFAProduction] -> String -> (String, Bool)
generateAbsSynDataType ps tokenType = ("data AbsSynToken" ++ typeParams ++ " = AbsSynToken (Token " ++ tokenType ++ ")" ++ constructors, all isJust resultTypes)
  where
    typeParams = (concat $ fmap (\(mt, n) -> if isJust mt then "" else " t" ++ n ) types)
    constructors = concat $ fmap (\(mt, n) -> " | AbsSynResult" ++ n ++ " (" ++ fromMaybe ('t':n) mt ++ ") ParseState") types
    uniqueProds = tail $ nubBy (on (==) _dfaProductionName) ps -- tail here as first production is START and we don't need a type for that
    resultTypes = fmap ((fmap trim) . _dfaProductionResultType) uniqueProds
    types = zip resultTypes $ fmap show [1..]

-- Call and concat generateStateCode on each state, retaining index
generateStatesCode :: [DFAState] -> [DFAProduction] -> TokenMap -> Bool -> String
generateStatesCode ss' ps tm isDefined = aux ss' 0
  where
    outType = fromMaybe "" $ _dfaProductionResultType $ head ps
    aux [] n = ""
    aux (s:ss) n = generateStateCode s n ps tm isDefined outType ++ "\n\n\n" ++ (aux ss $ n + 1)

-- Generate each state function
generateStateCode :: DFAState -> Int -> [DFAProduction] -> TokenMap -> Bool -> String -> String
generateStateCode (DFAState _ as' ) n ps tm isDefined outType = (if isDefined then stateType else "") ++ aux as'
  where
    stateType = stateName ++ " :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result " ++ outType ++ "\n"
    stateName = "generatedState" ++ show n
    aux [] = stateName ++ " _ _ _ xs = generatedError " ++ show n ++ " xs"
    aux ((t, a):as) = stateName ++ " ps0 vs ss (x@" ++ pattern ++ ":xs) = "
        ++ generateStateAction n a ps parseState ++ "\n"
        ++ aux as
      where
        (pattern, parseState) = generateStatePattern ps tm t

-- Generate the pattern for matching next tokens
generateStatePattern :: [DFAProduction] -> TokenMap -> RuleTokenType -> (String, String)
generateStatePattern ps tm (RuleTerminal t) = ("(AbsSynToken (Token ps (" ++ (replace "$$" "_" $ trim $ tokenPattern $ tm ! t) ++ ")))", "ps")
generateStatePattern ps tm (RuleNonTerminal t) = ("(AbsSynResult" ++ (show $ getProdIndex t ps) ++ " _ _)", "ps0")

-- Generate the action code for shifting, reducing or finishing
generateStateAction :: Int -> DFAAction -> [DFAProduction] -> String -> String
generateStateAction i DFAFinish ps _ = "return $ unpackFinal $ head vs"
generateStateAction i (DFAShift i') ps parseState = "generatedState" ++ (show i') ++ " " ++ parseState ++ " (x:vs) (" ++ (show i) ++ ":ss) xs"
generateStateAction i (DFAReduce i') ps parseState = nextStateStr ++ " " ++ parseState ++ " " ++ dropStrVs ++ " " ++ dropStr ++ " (x':x:xs)\n\
\  where x' = generatedReduction" ++ show i' ++ " ps0 vs"
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
generateReductions :: [DFAProduction] -> TokenMap -> Bool -> String
generateReductions ps tm isDefined = concat [(generateReduction v ps tm isDefined) ++ "\n\n" | v <- [1..length ps - 1] ]

-- Pattern match expected tokens for reduction, call result code from gmr file, pack result in correct AbsSynToken constructor
generateReduction :: Int -> [DFAProduction] -> TokenMap -> Bool -> String
generateReduction i ps tm isDefined = (if isDefined then reductionType else "") ++
    reductionName ++ " ps0 " ++ vPattern ++
    " = AbsSynResult" ++ (show $ getProdIndex (p^.dfaProductionName) ps) ++
    " (" ++ (p^.dfaProductionResult.to trim) ++ ") ps" ++ if count == 0 then "0" else "1"
  where
    reductionName = "generatedReduction" ++ (show i)
    reductionType = reductionName ++ " :: ParseState -> [AbsSynToken] -> AbsSynToken\n"
    p = ps !! i
    prodTokens = p^.dfaProductionTokens
    count = length prodTokens
    vPattern = "(" ++ (concat $ reverse [viPattern (show $ i + 1) (prodTokens !! i) ++ ":" | i <- [0..count - 1]]) ++ "_)"
    viPattern i' (RuleTerminal t) = concat ["(AbsSynToken (Token ps", i', " ", terminalPattern t i', "))"]
    viPattern i' (RuleNonTerminal prod) = concat ["(AbsSynResult", show $ getProdIndex prod ps, " v", i', " ps", i', ")"]
    terminalPattern t i' = if length patternSplit == 1 then v else "(" ++ intercalate v patternSplit ++ ")"
      where
        v = "v" ++ i'
        patternSplit = splitOn "$$" $ trim $ tokenPattern $ tm ! t