{-|
Module      : Cfgparser
Description : Parser generated with the Haskell Parser Generator - https://github.com/samuelWilliams99/haskell_parser_generator
-}
module Cfgparser (runParser, module ParserRequirements) where


import ParserRequirements
import Control.Applicative
import Grammar
import Data.Maybe
wrapCustom :: TokenDef -> TokenDef
wrapCustom t = t{tokenPattern="TokenCustom (" ++ tokenPattern t ++ ")"}

gScanner = Scanner{ separateCasedIdentifiers=True
                  , ignoreWhitespace=True
                  , ignoreComments=True
                  , operators=["::","|","*","+","?","=>"]
                  , keywords=[]
                  , lineComment=Just "#"
                  , blockComment=Just ("#[","]#")
                  , includeEOF=True
                  , parserMap=(languageDefsParser:)
                  }

-- | Generates the Abstract Syntax Tree from an input string, can fail
runParser str = do
    ts <- scan gScanner str
    let ps = if length ts == 0 then parseState "" else let (Token ps' _) = head ts in ps'
    generatedState0 ps [] [] $ fmap AbsSynToken ts

generatedError n [] = Error "Ran out of tokens"
generatedError n ((AbsSynToken (Token ps x)):xs) = Error $ "Unexpected token: " ++ (show x) ++ " at " ++ showPos ps

unpackFinal (AbsSynResult1 x _) = x


generatedStates = [generatedState0, generatedState1, generatedState2, generatedState3, generatedState4, generatedState5, generatedState6, generatedState7, generatedState8, generatedState9, generatedState10, generatedState11, generatedState12, generatedState13, generatedState14, generatedState15, generatedState16, generatedState17, generatedState18, generatedState19, generatedState20, generatedState21, generatedState22, generatedState23, generatedState24, generatedState25, generatedState26, generatedState27, generatedState28, generatedState29, generatedState30, generatedState31, generatedState32, generatedState33, generatedState34, generatedState35, generatedState36, generatedState37, generatedState38, generatedState39, generatedState40, generatedState41, generatedState42, generatedState43, generatedState44, generatedState45, generatedState46, generatedState47, generatedState48, generatedState49, generatedState50, generatedState51, generatedState52, generatedState53, generatedState54, generatedState55, generatedState56, generatedState57, generatedState58, generatedState59, generatedState60, generatedState61, generatedState62, generatedState63, generatedState64, generatedState65, generatedState66, generatedState67, generatedState68, generatedState69, generatedState70, generatedState71, generatedState72, generatedState73, generatedState74, generatedState75, generatedState76, generatedState77, generatedState78, generatedState79, generatedState80, generatedState81, generatedState82, generatedState83, generatedState84, generatedState85, generatedState86, generatedState87, generatedState88, generatedState89, generatedState90, generatedState91, generatedState92, generatedState93, generatedState94, generatedState95, generatedState96, generatedState97, generatedState98, generatedState99, generatedState100, generatedState101, generatedState102, generatedState103, generatedState104, generatedState105, generatedState106, generatedState107, generatedState108, generatedState109, generatedState110, generatedState111, generatedState112, generatedState113, generatedState114, generatedState115, generatedState116, generatedState117, generatedState118, generatedState119, generatedState120, generatedState121, generatedState122, generatedState123, generatedState124]

data AbsSynToken = AbsSynToken (Token LanguageDefs) | AbsSynResult1 ((Maybe String, String, [(String, [String])], Grammar)) ParseState | AbsSynResult2 (Maybe (String)) ParseState | AbsSynResult3 (Maybe (String)) ParseState | AbsSynResult4 (Maybe ( String )) ParseState | AbsSynResult5 (Maybe ( String )) ParseState | AbsSynResult6 ([ (String, [String]) ]) ParseState | AbsSynResult7 ([ (String, [String]) ]) ParseState | AbsSynResult8 (String) ParseState | AbsSynResult9 (String) ParseState | AbsSynResult10 (String) ParseState | AbsSynResult11 (Grammar) ParseState | AbsSynResult12 (Maybe ( [TokenDef] )) ParseState | AbsSynResult13 (Maybe ( [TokenDef] )) ParseState | AbsSynResult14 ([PrecLevel]) ParseState | AbsSynResult15 ([PrecLevel]) ParseState | AbsSynResult16 ([Rule]) ParseState | AbsSynResult17 ((String, [String])) ParseState | AbsSynResult18 ([String]) ParseState | AbsSynResult19 ([TokenDef]) ParseState | AbsSynResult20 ([TokenDef]) ParseState | AbsSynResult21 (String) ParseState | AbsSynResult22 (TokenDef) ParseState | AbsSynResult23 (Maybe (String)) ParseState | AbsSynResult24 (Maybe (String)) ParseState | AbsSynResult25 (PrecLevel) ParseState | AbsSynResult26 ([String]) ParseState | AbsSynResult27 (String) ParseState | AbsSynResult28 (Associativity) ParseState | AbsSynResult29 (Rule) ParseState | AbsSynResult30 ([RuleProduction]) ParseState | AbsSynResult31 (RuleProduction) ParseState | AbsSynResult32 ([RuleToken]) ParseState | AbsSynResult33 (RuleToken) ParseState | AbsSynResult34 (Maybe (RuleTokenType)) ParseState | AbsSynResult35 (Maybe (RuleTokenType)) ParseState | AbsSynResult36 (RuleTokenType) ParseState | AbsSynResult37 (RuleTokenType) ParseState

generatedState0 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "export" )))):xs) = generatedState1 ps (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "precode" )))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState3 ps0 (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult2 _ _):xs) = generatedState4 ps0 (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult3 _ _):xs) = generatedState113 ps0 (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult1 _ _):xs) = generatedState124 ps0 (x:vs) (0:ss) xs
generatedState0 _ _ _ xs = generatedError 0 xs


generatedState1 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState1 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState2 ps (x:vs) (1:ss) xs
generatedState1 _ _ _ xs = generatedError 1 xs


generatedState2 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "precode" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState2 _ _ _ xs = generatedError 2 xs


generatedState3 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "precode" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState3 _ _ _ xs = generatedError 3 xs


generatedState4 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "precode" )))):xs) = generatedState5 ps (x:vs) (4:ss) xs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = generatedState4 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState7 ps0 (x:vs) (4:ss) xs
generatedState4 ps0 vs ss (x@(AbsSynResult4 _ _):xs) = generatedState8 ps0 (x:vs) (4:ss) xs
generatedState4 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState108 ps0 (x:vs) (4:ss) xs
generatedState4 _ _ _ xs = generatedError 4 xs


generatedState5 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState6 ps (x:vs) (5:ss) xs
generatedState5 _ _ _ xs = generatedError 5 xs


generatedState6 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState6 _ _ _ xs = generatedError 6 xs


generatedState7 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState7 _ _ _ xs = generatedError 7 xs


generatedState8 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = generatedState9 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = generatedState15 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = generatedState16 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = generatedState17 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = generatedState18 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = generatedState21 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = generatedState23 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = generatedState27 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState8 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState8 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState8 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState8 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState8 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState8 ps0 vs ss (x@(AbsSynResult17 _ _):xs) = generatedState29 ps0 (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynResult6 _ _):xs) = generatedState31 ps0 (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState106 ps0 (x:vs) (8:ss) xs
generatedState8 _ _ _ xs = generatedError 8 xs


generatedState9 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState10 ps (x:vs) (9:ss) xs
generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState11 ps (x:vs) (9:ss) xs
generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState12 ps (x:vs) (9:ss) xs
generatedState9 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState13 ps0 (x:vs) (9:ss) xs
generatedState9 _ _ _ xs = generatedError 9 xs


generatedState10 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState10 _ _ _ xs = generatedError 10 xs


generatedState11 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState11 _ _ _ xs = generatedError 11 xs


generatedState12 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState12 _ _ _ xs = generatedError 12 xs


generatedState13 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState10 ps (x:vs) (13:ss) xs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState11 ps (x:vs) (13:ss) xs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState12 ps (x:vs) (13:ss) xs
generatedState13 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState14 ps0 (x:vs) (13:ss) xs
generatedState13 _ _ _ xs = generatedError 13 xs


generatedState14 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState14 _ _ _ xs = generatedError 14 xs


generatedState15 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState15 _ _ _ xs = generatedError 15 xs


generatedState16 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState16 _ _ _ xs = generatedError 16 xs


generatedState17 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState17 _ _ _ xs = generatedError 17 xs


generatedState18 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState19 ps (x:vs) (18:ss) xs
generatedState18 _ _ _ xs = generatedError 18 xs


generatedState19 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState20 ps (x:vs) (19:ss) xs
generatedState19 _ _ _ xs = generatedError 19 xs


generatedState20 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState20 _ _ _ xs = generatedError 20 xs


generatedState21 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState22 ps (x:vs) (21:ss) xs
generatedState21 _ _ _ xs = generatedError 21 xs


generatedState22 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState22 _ _ _ xs = generatedError 22 xs


generatedState23 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState24 ps (x:vs) (23:ss) xs
generatedState23 ps0 vs ss (x@(AbsSynResult18 _ _):xs) = generatedState26 ps0 (x:vs) (23:ss) xs
generatedState23 _ _ _ xs = generatedError 23 xs


generatedState24 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState24 ps (x:vs) (24:ss) xs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynResult18 _ _):xs) = generatedState25 ps0 (x:vs) (24:ss) xs
generatedState24 _ _ _ xs = generatedError 24 xs


generatedState25 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState25 _ _ _ xs = generatedError 25 xs


generatedState26 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState26 _ _ _ xs = generatedError 26 xs


generatedState27 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState27 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState24 ps (x:vs) (27:ss) xs
generatedState27 ps0 vs ss (x@(AbsSynResult18 _ _):xs) = generatedState28 ps0 (x:vs) (27:ss) xs
generatedState27 _ _ _ xs = generatedError 27 xs


generatedState28 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState28 _ _ _ xs = generatedError 28 xs


generatedState29 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = generatedState9 ps (x:vs) (29:ss) xs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = generatedState15 ps (x:vs) (29:ss) xs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = generatedState16 ps (x:vs) (29:ss) xs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = generatedState17 ps (x:vs) (29:ss) xs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = generatedState18 ps (x:vs) (29:ss) xs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = generatedState21 ps (x:vs) (29:ss) xs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = generatedState23 ps (x:vs) (29:ss) xs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = generatedState27 ps (x:vs) (29:ss) xs
generatedState29 ps0 vs ss (x@(AbsSynResult17 _ _):xs) = generatedState29 ps0 (x:vs) (29:ss) xs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState30 ps0 (x:vs) (29:ss) xs
generatedState29 _ _ _ xs = generatedError 29 xs


generatedState30 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction14 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction14 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction14 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction14 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction14 ps0 vs
generatedState30 _ _ _ xs = generatedError 30 xs


generatedState31 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState32 ps (x:vs) (31:ss) xs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState31 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState31 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState31 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState31 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState31 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynResult19 _ _):xs) = generatedState47 ps0 (x:vs) (31:ss) xs
generatedState31 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState48 ps0 (x:vs) (31:ss) xs
generatedState31 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState100 ps0 (x:vs) (31:ss) xs
generatedState31 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState105 ps0 (x:vs) (31:ss) xs
generatedState31 _ _ _ xs = generatedError 31 xs


generatedState32 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState33 ps (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState40 ps (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynResult22 _ _):xs) = generatedState44 ps0 (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynResult20 _ _):xs) = generatedState46 ps0 (x:vs) (32:ss) xs
generatedState32 _ _ _ xs = generatedError 32 xs


generatedState33 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState34 ps (x:vs) (33:ss) xs
generatedState33 _ _ _ xs = generatedError 33 xs


generatedState34 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = generatedState35 ps (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState34 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState34 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState34 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState34 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState34 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState34 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState34 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState34 ps0 vs ss (x@(AbsSynResult21 _ _):xs) = generatedState37 ps0 (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynResult23 _ _):xs) = generatedState38 ps0 (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState39 ps0 (x:vs) (34:ss) xs
generatedState34 _ _ _ xs = generatedError 34 xs


generatedState35 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState10 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState11 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState12 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState36 ps0 (x:vs) (35:ss) xs
generatedState35 _ _ _ xs = generatedError 35 xs


generatedState36 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 ps0 vs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 ps0 vs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 ps0 vs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 ps0 vs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 ps0 vs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 ps0 vs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 ps0 vs
generatedState36 _ _ _ xs = generatedError 36 xs


generatedState37 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 ps0 vs
generatedState37 _ _ _ xs = generatedError 37 xs


generatedState38 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 ps0 vs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 ps0 vs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 ps0 vs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 ps0 vs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 ps0 vs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 ps0 vs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 ps0 vs
generatedState38 _ _ _ xs = generatedError 38 xs


generatedState39 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction48 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction48 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction48 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction48 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction48 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction48 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction48 ps0 vs
generatedState39 _ _ _ xs = generatedError 39 xs


generatedState40 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState40 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState41 ps (x:vs) (40:ss) xs
generatedState40 _ _ _ xs = generatedError 40 xs


generatedState41 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = generatedState35 ps (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState41 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState41 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState41 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState41 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState41 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState41 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState41 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynResult21 _ _):xs) = generatedState37 ps0 (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynResult23 _ _):xs) = generatedState42 ps0 (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState43 ps0 (x:vs) (41:ss) xs
generatedState41 _ _ _ xs = generatedError 41 xs


generatedState42 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState42 _ _ _ xs = generatedError 42 xs


generatedState43 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState43 _ _ _ xs = generatedError 43 xs


generatedState44 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState33 ps (x:vs) (44:ss) xs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState40 ps (x:vs) (44:ss) xs
generatedState44 ps0 vs ss (x@(AbsSynResult22 _ _):xs) = generatedState44 ps0 (x:vs) (44:ss) xs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynResult20 _ _):xs) = generatedState45 ps0 (x:vs) (44:ss) xs
generatedState44 _ _ _ xs = generatedError 44 xs


generatedState45 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState45 _ _ _ xs = generatedError 45 xs


generatedState46 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState46 _ _ _ xs = generatedError 46 xs


generatedState47 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState47 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState47 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState47 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState47 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState47 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState47 _ _ _ xs = generatedError 47 xs


generatedState48 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState48 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState49 ps (x:vs) (48:ss) xs
generatedState48 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState50 ps (x:vs) (48:ss) xs
generatedState48 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState51 ps (x:vs) (48:ss) xs
generatedState48 ps0 vs ss (x@(AbsSynResult28 _ _):xs) = generatedState52 ps0 (x:vs) (48:ss) xs
generatedState48 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState48 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState48 ps0 vs ss (x@(AbsSynResult25 _ _):xs) = generatedState58 ps0 (x:vs) (48:ss) xs
generatedState48 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState60 ps0 (x:vs) (48:ss) xs
generatedState48 ps0 vs ss (x@(AbsSynResult15 _ _):xs) = generatedState98 ps0 (x:vs) (48:ss) xs
generatedState48 _ _ _ xs = generatedError 48 xs


generatedState49 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState49 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction59 ps0 vs
generatedState49 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction59 ps0 vs
generatedState49 _ _ _ xs = generatedError 49 xs


generatedState50 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction58 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction58 ps0 vs
generatedState50 _ _ _ xs = generatedError 50 xs


generatedState51 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState51 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction57 ps0 vs
generatedState51 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction57 ps0 vs
generatedState51 _ _ _ xs = generatedError 51 xs


generatedState52 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState53 ps (x:vs) (52:ss) xs
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState54 ps (x:vs) (52:ss) xs
generatedState52 ps0 vs ss (x@(AbsSynResult27 _ _):xs) = generatedState55 ps0 (x:vs) (52:ss) xs
generatedState52 ps0 vs ss (x@(AbsSynResult26 _ _):xs) = generatedState57 ps0 (x:vs) (52:ss) xs
generatedState52 _ _ _ xs = generatedError 52 xs


generatedState53 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 _ _ _ xs = generatedError 53 xs


generatedState54 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState54 _ _ _ xs = generatedError 54 xs


generatedState55 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState53 ps (x:vs) (55:ss) xs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState54 ps (x:vs) (55:ss) xs
generatedState55 ps0 vs ss (x@(AbsSynResult27 _ _):xs) = generatedState55 ps0 (x:vs) (55:ss) xs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynResult26 _ _):xs) = generatedState56 ps0 (x:vs) (55:ss) xs
generatedState55 _ _ _ xs = generatedError 55 xs


generatedState56 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState56 _ _ _ xs = generatedError 56 xs


generatedState57 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction52 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction52 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction52 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction52 ps0 vs
generatedState57 _ _ _ xs = generatedError 57 xs


generatedState58 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState49 ps (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState50 ps (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState51 ps (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynResult28 _ _):xs) = generatedState52 ps0 (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynResult25 _ _):xs) = generatedState58 ps0 (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState58 ps0 vs ss (x@(AbsSynResult15 _ _):xs) = generatedState59 ps0 (x:vs) (58:ss) xs
generatedState58 _ _ _ xs = generatedError 58 xs


generatedState59 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState59 _ _ _ xs = generatedError 59 xs


generatedState60 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState61 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynResult29 _ _):xs) = generatedState95 ps0 (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState97 ps0 (x:vs) (60:ss) xs
generatedState60 _ _ _ xs = generatedError 60 xs


generatedState61 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState61 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "::"))):xs) = generatedState62 ps (x:vs) (61:ss) xs
generatedState61 _ _ _ xs = generatedError 61 xs


generatedState62 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState63 ps (x:vs) (62:ss) xs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState64 ps (x:vs) (62:ss) xs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState65 ps (x:vs) (62:ss) xs
generatedState62 ps0 vs ss (x@(AbsSynResult37 _ _):xs) = generatedState66 ps0 (x:vs) (62:ss) xs
generatedState62 ps0 vs ss (x@(AbsSynResult33 _ _):xs) = generatedState78 ps0 (x:vs) (62:ss) xs
generatedState62 ps0 vs ss (x@(AbsSynResult32 _ _):xs) = generatedState80 ps0 (x:vs) (62:ss) xs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "empty" )))):xs) = generatedState87 ps (x:vs) (62:ss) xs
generatedState62 ps0 vs ss (x@(AbsSynResult31 _ _):xs) = generatedState89 ps0 (x:vs) (62:ss) xs
generatedState62 ps0 vs ss (x@(AbsSynResult30 _ _):xs) = generatedState92 ps0 (x:vs) (62:ss) xs
generatedState62 _ _ _ xs = generatedError 62 xs


generatedState63 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction81 ps0 vs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "?"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction81 ps0 vs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction81 ps0 vs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction81 ps0 vs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction81 ps0 vs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction81 ps0 vs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction81 ps0 vs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction81 ps0 vs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction81 ps0 vs
generatedState63 _ _ _ xs = generatedError 63 xs


generatedState64 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction80 ps0 vs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "?"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction80 ps0 vs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction80 ps0 vs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction80 ps0 vs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction80 ps0 vs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction80 ps0 vs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction80 ps0 vs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction80 ps0 vs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction80 ps0 vs
generatedState64 _ _ _ xs = generatedError 64 xs


generatedState65 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction79 ps0 vs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "?"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction79 ps0 vs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction79 ps0 vs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction79 ps0 vs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction79 ps0 vs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction79 ps0 vs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction79 ps0 vs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction79 ps0 vs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction79 ps0 vs
generatedState65 _ _ _ xs = generatedError 65 xs


generatedState66 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "?"))):xs) = generatedState67 ps (x:vs) (66:ss) xs
generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState68 ps (x:vs) (66:ss) xs
generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState75 ps (x:vs) (66:ss) xs
generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 ps0 vs
generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 ps0 vs
generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 ps0 vs
generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 ps0 vs
generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 ps0 vs
generatedState66 _ _ _ xs = generatedError 66 xs


generatedState67 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState67 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState67 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState67 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState67 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState67 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState67 _ _ _ xs = generatedError 67 xs


generatedState68 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState69 ps (x:vs) (68:ss) xs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynResult36 _ _):xs) = generatedState72 ps0 (x:vs) (68:ss) xs
generatedState68 ps0 vs ss (x@(AbsSynResult34 _ _):xs) = generatedState73 ps0 (x:vs) (68:ss) xs
generatedState68 ps0 vs ss (x@(AbsSynResult35 _ _):xs) = generatedState74 ps0 (x:vs) (68:ss) xs
generatedState68 _ _ _ xs = generatedError 68 xs


generatedState69 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState63 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState64 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState65 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynResult37 _ _):xs) = generatedState70 ps0 (x:vs) (69:ss) xs
generatedState69 _ _ _ xs = generatedError 69 xs


generatedState70 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = generatedState71 ps (x:vs) (70:ss) xs
generatedState70 _ _ _ xs = generatedError 70 xs


generatedState71 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction78 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction78 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction78 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction78 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction78 ps0 vs
generatedState71 _ _ _ xs = generatedError 71 xs


generatedState72 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction77 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction77 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction77 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction77 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction77 ps0 vs
generatedState72 _ _ _ xs = generatedError 72 xs


generatedState73 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState73 _ _ _ xs = generatedError 73 xs


generatedState74 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState74 _ _ _ xs = generatedError 74 xs


generatedState75 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState69 ps (x:vs) (75:ss) xs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = generatedState75 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState75 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState75 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState75 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState75 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynResult36 _ _):xs) = generatedState72 ps0 (x:vs) (75:ss) xs
generatedState75 ps0 vs ss (x@(AbsSynResult34 _ _):xs) = generatedState76 ps0 (x:vs) (75:ss) xs
generatedState75 ps0 vs ss (x@(AbsSynResult35 _ _):xs) = generatedState77 ps0 (x:vs) (75:ss) xs
generatedState75 _ _ _ xs = generatedError 75 xs


generatedState76 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState76 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction72 ps0 vs
generatedState76 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction72 ps0 vs
generatedState76 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction72 ps0 vs
generatedState76 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction72 ps0 vs
generatedState76 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction72 ps0 vs
generatedState76 _ _ _ xs = generatedError 76 xs


generatedState77 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction71 ps0 vs
generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction71 ps0 vs
generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction71 ps0 vs
generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction71 ps0 vs
generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction71 ps0 vs
generatedState77 _ _ _ xs = generatedError 77 xs


generatedState78 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState78 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState63 ps (x:vs) (78:ss) xs
generatedState78 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState64 ps (x:vs) (78:ss) xs
generatedState78 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState65 ps (x:vs) (78:ss) xs
generatedState78 ps0 vs ss (x@(AbsSynResult37 _ _):xs) = generatedState66 ps0 (x:vs) (78:ss) xs
generatedState78 ps0 vs ss (x@(AbsSynResult33 _ _):xs) = generatedState78 ps0 (x:vs) (78:ss) xs
generatedState78 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 ps0 vs
generatedState78 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 ps0 vs
generatedState78 ps0 vs ss (x@(AbsSynResult32 _ _):xs) = generatedState79 ps0 (x:vs) (78:ss) xs
generatedState78 _ _ _ xs = generatedError 78 xs


generatedState79 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState79 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction68 ps0 vs
generatedState79 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction68 ps0 vs
generatedState79 _ _ _ xs = generatedError 79 xs


generatedState80 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState80 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "prec" )))):xs) = generatedState81 ps (x:vs) (80:ss) xs
generatedState80 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState86 ps (x:vs) (80:ss) xs
generatedState80 _ _ _ xs = generatedError 80 xs


generatedState81 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState81 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState82 ps (x:vs) (81:ss) xs
generatedState81 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState84 ps (x:vs) (81:ss) xs
generatedState81 _ _ _ xs = generatedError 81 xs


generatedState82 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState83 ps (x:vs) (82:ss) xs
generatedState82 _ _ _ xs = generatedError 82 xs


generatedState83 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction67 ps0 vs
generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction67 ps0 vs
generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction67 ps0 vs
generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "|"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction67 ps0 vs
generatedState83 _ _ _ xs = generatedError 83 xs


generatedState84 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState84 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState85 ps (x:vs) (84:ss) xs
generatedState84 _ _ _ xs = generatedError 84 xs


generatedState85 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState85 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction66 ps0 vs
generatedState85 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction66 ps0 vs
generatedState85 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction66 ps0 vs
generatedState85 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "|"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction66 ps0 vs
generatedState85 _ _ _ xs = generatedError 85 xs


generatedState86 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState86 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction65 ps0 vs
generatedState86 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction65 ps0 vs
generatedState86 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction65 ps0 vs
generatedState86 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "|"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction65 ps0 vs
generatedState86 _ _ _ xs = generatedError 86 xs


generatedState87 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState87 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenCodeBlock _ )))):xs) = generatedState88 ps (x:vs) (87:ss) xs
generatedState87 _ _ _ xs = generatedError 87 xs


generatedState88 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState88 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState88 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState88 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState88 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "|"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState88 _ _ _ xs = generatedError 88 xs


generatedState89 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState89 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState89 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState89 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState89 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "|"))):xs) = generatedState90 ps (x:vs) (89:ss) xs
generatedState89 _ _ _ xs = generatedError 89 xs


generatedState90 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState90 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState63 ps (x:vs) (90:ss) xs
generatedState90 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState64 ps (x:vs) (90:ss) xs
generatedState90 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState65 ps (x:vs) (90:ss) xs
generatedState90 ps0 vs ss (x@(AbsSynResult37 _ _):xs) = generatedState66 ps0 (x:vs) (90:ss) xs
generatedState90 ps0 vs ss (x@(AbsSynResult33 _ _):xs) = generatedState78 ps0 (x:vs) (90:ss) xs
generatedState90 ps0 vs ss (x@(AbsSynResult32 _ _):xs) = generatedState80 ps0 (x:vs) (90:ss) xs
generatedState90 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "empty" )))):xs) = generatedState87 ps (x:vs) (90:ss) xs
generatedState90 ps0 vs ss (x@(AbsSynResult31 _ _):xs) = generatedState89 ps0 (x:vs) (90:ss) xs
generatedState90 ps0 vs ss (x@(AbsSynResult30 _ _):xs) = generatedState91 ps0 (x:vs) (90:ss) xs
generatedState90 _ _ _ xs = generatedError 90 xs


generatedState91 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState91 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState91 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState91 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState91 _ _ _ xs = generatedError 91 xs


generatedState92 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = generatedState35 ps (x:vs) (92:ss) xs
generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState92 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState92 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState92 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState92 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState92 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState92 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState92 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState92 ps0 vs ss (x@(AbsSynResult21 _ _):xs) = generatedState37 ps0 (x:vs) (92:ss) xs
generatedState92 ps0 vs ss (x@(AbsSynResult23 _ _):xs) = generatedState93 ps0 (x:vs) (92:ss) xs
generatedState92 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState94 ps0 (x:vs) (92:ss) xs
generatedState92 _ _ _ xs = generatedError 92 xs


generatedState93 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState93 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState93 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState93 _ _ _ xs = generatedError 93 xs


generatedState94 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState94 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction60 ps0 vs
generatedState94 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction60 ps0 vs
generatedState94 _ _ _ xs = generatedError 94 xs


generatedState95 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState95 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState61 ps (x:vs) (95:ss) xs
generatedState95 ps0 vs ss (x@(AbsSynResult29 _ _):xs) = generatedState95 ps0 (x:vs) (95:ss) xs
generatedState95 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState95 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState96 ps0 (x:vs) (95:ss) xs
generatedState95 _ _ _ xs = generatedError 95 xs


generatedState96 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState96 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState96 _ _ _ xs = generatedError 96 xs


generatedState97 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState97 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState97 _ _ _ xs = generatedError 97 xs


generatedState98 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState98 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState61 ps (x:vs) (98:ss) xs
generatedState98 ps0 vs ss (x@(AbsSynResult29 _ _):xs) = generatedState95 ps0 (x:vs) (98:ss) xs
generatedState98 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState99 ps0 (x:vs) (98:ss) xs
generatedState98 _ _ _ xs = generatedError 98 xs


generatedState99 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState99 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState99 _ _ _ xs = generatedError 99 xs


generatedState100 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState100 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState49 ps (x:vs) (100:ss) xs
generatedState100 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState50 ps (x:vs) (100:ss) xs
generatedState100 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState51 ps (x:vs) (100:ss) xs
generatedState100 ps0 vs ss (x@(AbsSynResult28 _ _):xs) = generatedState52 ps0 (x:vs) (100:ss) xs
generatedState100 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState100 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState100 ps0 vs ss (x@(AbsSynResult25 _ _):xs) = generatedState58 ps0 (x:vs) (100:ss) xs
generatedState100 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState101 ps0 (x:vs) (100:ss) xs
generatedState100 ps0 vs ss (x@(AbsSynResult15 _ _):xs) = generatedState103 ps0 (x:vs) (100:ss) xs
generatedState100 _ _ _ xs = generatedError 100 xs


generatedState101 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState101 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState61 ps (x:vs) (101:ss) xs
generatedState101 ps0 vs ss (x@(AbsSynResult29 _ _):xs) = generatedState95 ps0 (x:vs) (101:ss) xs
generatedState101 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState102 ps0 (x:vs) (101:ss) xs
generatedState101 _ _ _ xs = generatedError 101 xs


generatedState102 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState102 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState102 _ _ _ xs = generatedError 102 xs


generatedState103 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState103 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState61 ps (x:vs) (103:ss) xs
generatedState103 ps0 vs ss (x@(AbsSynResult29 _ _):xs) = generatedState95 ps0 (x:vs) (103:ss) xs
generatedState103 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState104 ps0 (x:vs) (103:ss) xs
generatedState103 _ _ _ xs = generatedError 103 xs


generatedState104 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState104 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState104 _ _ _ xs = generatedError 104 xs


generatedState105 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState105 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction8 ps0 vs
generatedState105 _ _ _ xs = generatedError 105 xs


generatedState106 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState106 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState32 ps (x:vs) (106:ss) xs
generatedState106 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState106 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState106 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState106 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState106 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState106 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState106 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState106 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState106 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState106 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState106 ps0 vs ss (x@(AbsSynResult19 _ _):xs) = generatedState47 ps0 (x:vs) (106:ss) xs
generatedState106 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState48 ps0 (x:vs) (106:ss) xs
generatedState106 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState100 ps0 (x:vs) (106:ss) xs
generatedState106 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState107 ps0 (x:vs) (106:ss) xs
generatedState106 _ _ _ xs = generatedError 106 xs


generatedState107 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState107 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction7 ps0 vs
generatedState107 _ _ _ xs = generatedError 107 xs


generatedState108 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = generatedState9 ps (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = generatedState15 ps (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = generatedState16 ps (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = generatedState17 ps (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = generatedState18 ps (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = generatedState21 ps (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = generatedState23 ps (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = generatedState27 ps (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState108 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState108 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState108 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState108 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState108 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState108 ps0 vs ss (x@(AbsSynResult17 _ _):xs) = generatedState29 ps0 (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynResult6 _ _):xs) = generatedState109 ps0 (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState111 ps0 (x:vs) (108:ss) xs
generatedState108 _ _ _ xs = generatedError 108 xs


generatedState109 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState109 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState32 ps (x:vs) (109:ss) xs
generatedState109 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState109 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState109 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState109 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState109 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState109 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState109 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState109 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState109 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState109 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState109 ps0 vs ss (x@(AbsSynResult19 _ _):xs) = generatedState47 ps0 (x:vs) (109:ss) xs
generatedState109 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState48 ps0 (x:vs) (109:ss) xs
generatedState109 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState100 ps0 (x:vs) (109:ss) xs
generatedState109 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState110 ps0 (x:vs) (109:ss) xs
generatedState109 _ _ _ xs = generatedError 109 xs


generatedState110 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState110 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState110 _ _ _ xs = generatedError 110 xs


generatedState111 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState111 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState32 ps (x:vs) (111:ss) xs
generatedState111 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState111 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState111 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState111 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState111 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState111 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState111 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState111 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState111 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState111 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState111 ps0 vs ss (x@(AbsSynResult19 _ _):xs) = generatedState47 ps0 (x:vs) (111:ss) xs
generatedState111 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState48 ps0 (x:vs) (111:ss) xs
generatedState111 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState100 ps0 (x:vs) (111:ss) xs
generatedState111 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState112 ps0 (x:vs) (111:ss) xs
generatedState111 _ _ _ xs = generatedError 111 xs


generatedState112 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState112 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction5 ps0 vs
generatedState112 _ _ _ xs = generatedError 112 xs


generatedState113 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "precode" )))):xs) = generatedState5 ps (x:vs) (113:ss) xs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState7 ps0 (x:vs) (113:ss) xs
generatedState113 ps0 vs ss (x@(AbsSynResult4 _ _):xs) = generatedState114 ps0 (x:vs) (113:ss) xs
generatedState113 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState119 ps0 (x:vs) (113:ss) xs
generatedState113 _ _ _ xs = generatedError 113 xs


generatedState114 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = generatedState9 ps (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = generatedState15 ps (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = generatedState16 ps (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = generatedState17 ps (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = generatedState18 ps (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = generatedState21 ps (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = generatedState23 ps (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = generatedState27 ps (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState114 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState114 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState114 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState114 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState114 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState114 ps0 vs ss (x@(AbsSynResult17 _ _):xs) = generatedState29 ps0 (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynResult6 _ _):xs) = generatedState115 ps0 (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState117 ps0 (x:vs) (114:ss) xs
generatedState114 _ _ _ xs = generatedError 114 xs


generatedState115 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState115 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState32 ps (x:vs) (115:ss) xs
generatedState115 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState115 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState115 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState115 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState115 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState115 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState115 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState115 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState115 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState115 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState115 ps0 vs ss (x@(AbsSynResult19 _ _):xs) = generatedState47 ps0 (x:vs) (115:ss) xs
generatedState115 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState48 ps0 (x:vs) (115:ss) xs
generatedState115 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState100 ps0 (x:vs) (115:ss) xs
generatedState115 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState116 ps0 (x:vs) (115:ss) xs
generatedState115 _ _ _ xs = generatedError 115 xs


generatedState116 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState116 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction4 ps0 vs
generatedState116 _ _ _ xs = generatedError 116 xs


generatedState117 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState117 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState32 ps (x:vs) (117:ss) xs
generatedState117 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState117 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState117 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState117 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState117 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState117 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState117 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState117 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState117 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState117 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState117 ps0 vs ss (x@(AbsSynResult19 _ _):xs) = generatedState47 ps0 (x:vs) (117:ss) xs
generatedState117 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState48 ps0 (x:vs) (117:ss) xs
generatedState117 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState100 ps0 (x:vs) (117:ss) xs
generatedState117 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState118 ps0 (x:vs) (117:ss) xs
generatedState117 _ _ _ xs = generatedError 117 xs


generatedState118 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState118 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction3 ps0 vs
generatedState118 _ _ _ xs = generatedError 118 xs


generatedState119 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "parsermap" )))):xs) = generatedState9 ps (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepcomments" )))):xs) = generatedState15 ps (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keepwhitespaces" )))):xs) = generatedState16 ps (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "separateidentitycase" )))):xs) = generatedState17 ps (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "blockcomments" )))):xs) = generatedState18 ps (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "linecomments" )))):xs) = generatedState21 ps (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "keywords" )))):xs) = generatedState23 ps (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "operators" )))):xs) = generatedState27 ps (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState119 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState119 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState119 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState119 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState119 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState119 ps0 vs ss (x@(AbsSynResult17 _ _):xs) = generatedState29 ps0 (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynResult6 _ _):xs) = generatedState120 ps0 (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState122 ps0 (x:vs) (119:ss) xs
generatedState119 _ _ _ xs = generatedError 119 xs


generatedState120 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState120 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState32 ps (x:vs) (120:ss) xs
generatedState120 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState120 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState120 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState120 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState120 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState120 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState120 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState120 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState120 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState120 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState120 ps0 vs ss (x@(AbsSynResult19 _ _):xs) = generatedState47 ps0 (x:vs) (120:ss) xs
generatedState120 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState48 ps0 (x:vs) (120:ss) xs
generatedState120 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState100 ps0 (x:vs) (120:ss) xs
generatedState120 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState121 ps0 (x:vs) (120:ss) xs
generatedState120 _ _ _ xs = generatedError 120 xs


generatedState121 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState121 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction2 ps0 vs
generatedState121 _ _ _ xs = generatedError 121 xs


generatedState122 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState122 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "customtoken" )))):xs) = generatedState32 ps (x:vs) (122:ss) xs
generatedState122 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState122 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState122 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState122 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState122 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "left" )))):xs) = generatedState122 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState122 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "right" )))):xs) = generatedState122 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState122 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom ( TokenDirective "nonassoc" )))):xs) = generatedState122 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState122 ps0 vs ss (x@(AbsSynResult19 _ _):xs) = generatedState47 ps0 (x:vs) (122:ss) xs
generatedState122 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState48 ps0 (x:vs) (122:ss) xs
generatedState122 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState100 ps0 (x:vs) (122:ss) xs
generatedState122 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState123 ps0 (x:vs) (122:ss) xs
generatedState122 _ _ _ xs = generatedError 122 xs


generatedState123 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState123 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction1 ps0 vs
generatedState123 _ _ _ xs = generatedError 123 xs


generatedState124 :: ParseState -> [AbsSynToken] -> [Int] -> [AbsSynToken] -> Result  (Maybe String, String, [(String, [String])], Grammar) 
generatedState124 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = return $ unpackFinal $ head vs
generatedState124 _ _ _ xs = generatedError 124 xs


generatedReduction1 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction1 ps0 ((AbsSynResult11 v4 ps4):(AbsSynResult7 v3 ps3):(AbsSynResult5 v2 ps2):(AbsSynResult3 v1 ps1):_) = AbsSynResult1 ((v1, fromMaybe "" v2, v3, v4)) ps1

generatedReduction2 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction2 ps0 ((AbsSynResult11 v4 ps4):(AbsSynResult6 v3 ps3):(AbsSynResult5 v2 ps2):(AbsSynResult3 v1 ps1):_) = AbsSynResult1 ((v1, fromMaybe "" v2, v3, v4)) ps1

generatedReduction3 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction3 ps0 ((AbsSynResult11 v4 ps4):(AbsSynResult7 v3 ps3):(AbsSynResult4 v2 ps2):(AbsSynResult3 v1 ps1):_) = AbsSynResult1 ((v1, fromMaybe "" v2, v3, v4)) ps1

generatedReduction4 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction4 ps0 ((AbsSynResult11 v4 ps4):(AbsSynResult6 v3 ps3):(AbsSynResult4 v2 ps2):(AbsSynResult3 v1 ps1):_) = AbsSynResult1 ((v1, fromMaybe "" v2, v3, v4)) ps1

generatedReduction5 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction5 ps0 ((AbsSynResult11 v4 ps4):(AbsSynResult7 v3 ps3):(AbsSynResult5 v2 ps2):(AbsSynResult2 v1 ps1):_) = AbsSynResult1 ((v1, fromMaybe "" v2, v3, v4)) ps1

generatedReduction6 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction6 ps0 ((AbsSynResult11 v4 ps4):(AbsSynResult6 v3 ps3):(AbsSynResult5 v2 ps2):(AbsSynResult2 v1 ps1):_) = AbsSynResult1 ((v1, fromMaybe "" v2, v3, v4)) ps1

generatedReduction7 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction7 ps0 ((AbsSynResult11 v4 ps4):(AbsSynResult7 v3 ps3):(AbsSynResult4 v2 ps2):(AbsSynResult2 v1 ps1):_) = AbsSynResult1 ((v1, fromMaybe "" v2, v3, v4)) ps1

generatedReduction8 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction8 ps0 ((AbsSynResult11 v4 ps4):(AbsSynResult6 v3 ps3):(AbsSynResult4 v2 ps2):(AbsSynResult2 v1 ps1):_) = AbsSynResult1 ((v1, fromMaybe "" v2, v3, v4)) ps1

generatedReduction9 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction9 ps0 (_) = AbsSynResult2 (empty) ps0

generatedReduction10 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction10 ps0 ((AbsSynResult9 v1 ps1):_) = AbsSynResult3 (Just v1) ps1

generatedReduction11 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction11 ps0 (_) = AbsSynResult4 (empty) ps0

generatedReduction12 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction12 ps0 ((AbsSynResult10 v1 ps1):_) = AbsSynResult5 (Just v1) ps1

generatedReduction13 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction13 ps0 (_) = AbsSynResult6 (empty) ps0

generatedReduction14 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction14 ps0 ((AbsSynResult7 v2 ps2):(AbsSynResult17 v1 ps1):_) = AbsSynResult7 (v1:v2) ps1

generatedReduction15 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction15 ps0 ((AbsSynResult17 v1 ps1):_) = AbsSynResult7 ([v1]) ps1

generatedReduction16 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction16 ps0 ((AbsSynToken (Token ps1 (TokenCustom ( TokenCodeBlock v1 )))):_) = AbsSynResult8 (v1) ps1

generatedReduction17 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction17 ps0 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult8 (v1) ps1

generatedReduction18 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction18 ps0 ((AbsSynToken (Token ps1 (TokenUpperIdentifier v1))):_) = AbsSynResult8 (v1) ps1

generatedReduction19 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction19 ps0 ((AbsSynToken (Token ps2 (TokenCustom ( TokenCodeBlock v2 )))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult9 (v2) ps1

generatedReduction20 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction20 ps0 ((AbsSynToken (Token ps2 (TokenCustom ( TokenCodeBlock v2 )))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult10 (v2) ps1

generatedReduction21 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction21 ps0 ((AbsSynResult16 v3 ps3):(AbsSynResult15 v2 ps2):(AbsSynResult13 v1 ps1):_) = AbsSynResult11 (Grammar (fmap wrapCustom $ fromMaybe [] v1) v2 v3) ps1

generatedReduction22 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction22 ps0 ((AbsSynResult16 v3 ps3):(AbsSynResult14 v2 ps2):(AbsSynResult13 v1 ps1):_) = AbsSynResult11 (Grammar (fmap wrapCustom $ fromMaybe [] v1) v2 v3) ps1

generatedReduction23 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction23 ps0 ((AbsSynResult16 v3 ps3):(AbsSynResult15 v2 ps2):(AbsSynResult12 v1 ps1):_) = AbsSynResult11 (Grammar (fmap wrapCustom $ fromMaybe [] v1) v2 v3) ps1

generatedReduction24 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction24 ps0 ((AbsSynResult16 v3 ps3):(AbsSynResult14 v2 ps2):(AbsSynResult12 v1 ps1):_) = AbsSynResult11 (Grammar (fmap wrapCustom $ fromMaybe [] v1) v2 v3) ps1

generatedReduction25 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction25 ps0 (_) = AbsSynResult12 (empty) ps0

generatedReduction26 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction26 ps0 ((AbsSynResult19 v1 ps1):_) = AbsSynResult13 (Just v1) ps1

generatedReduction27 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction27 ps0 (_) = AbsSynResult14 (empty) ps0

generatedReduction28 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction28 ps0 ((AbsSynResult15 v2 ps2):(AbsSynResult25 v1 ps1):_) = AbsSynResult15 (v1:v2) ps1

generatedReduction29 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction29 ps0 ((AbsSynResult25 v1 ps1):_) = AbsSynResult15 ([v1]) ps1

generatedReduction30 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction30 ps0 ((AbsSynResult16 v2 ps2):(AbsSynResult29 v1 ps1):_) = AbsSynResult16 (v1:v2) ps1

generatedReduction31 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction31 ps0 ((AbsSynResult29 v1 ps1):_) = AbsSynResult16 ([v1]) ps1

generatedReduction32 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction32 ps0 ((AbsSynResult18 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (("ops", v2)) ps1

generatedReduction33 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction33 ps0 ((AbsSynResult18 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (("kwds", v2)) ps1

generatedReduction34 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction34 ps0 ((AbsSynToken (Token ps2 (TokenStringLit v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (("line", [v2])) ps1

generatedReduction35 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction35 ps0 ((AbsSynToken (Token ps3 (TokenStringLit v3))):(AbsSynToken (Token ps2 (TokenStringLit v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (("block", [v2, v3])) ps1

generatedReduction36 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction36 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (("sepiden", [])) ps1

generatedReduction37 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction37 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (("keepspace", [])) ps1

generatedReduction38 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction38 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (("keepcmts", [])) ps1

generatedReduction39 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction39 ps0 ((AbsSynResult8 v3 ps3):(AbsSynResult8 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (("parser", [v2, v3])) ps1

generatedReduction40 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction40 ps0 ((AbsSynResult18 v2 ps2):(AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult18 (v1:v2) ps1

generatedReduction41 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction41 ps0 ((AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult18 ([v1]) ps1

generatedReduction42 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction42 ps0 ((AbsSynResult20 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult19 (v2) ps1

generatedReduction43 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction43 ps0 ((AbsSynResult20 v2 ps2):(AbsSynResult22 v1 ps1):_) = AbsSynResult20 (v1:v2) ps1

generatedReduction44 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction44 ps0 ((AbsSynResult22 v1 ps1):_) = AbsSynResult20 ([v1]) ps1

generatedReduction45 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction45 ps0 ((AbsSynResult8 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult21 (v2) ps1

generatedReduction46 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction46 ps0 ((AbsSynResult24 v3 ps3):(AbsSynToken (Token ps2 (TokenCustom ( TokenCodeBlock v2 )))):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult22 (TokenDef v1 v2 v3) ps1

generatedReduction47 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction47 ps0 ((AbsSynResult23 v3 ps3):(AbsSynToken (Token ps2 (TokenCustom ( TokenCodeBlock v2 )))):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult22 (TokenDef v1 v2 v3) ps1

generatedReduction48 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction48 ps0 ((AbsSynResult24 v3 ps3):(AbsSynToken (Token ps2 (TokenCustom ( TokenCodeBlock v2 )))):(AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult22 (TokenDef v1 v2 v3) ps1

generatedReduction49 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction49 ps0 ((AbsSynResult23 v3 ps3):(AbsSynToken (Token ps2 (TokenCustom ( TokenCodeBlock v2 )))):(AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult22 (TokenDef v1 v2 v3) ps1

generatedReduction50 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction50 ps0 (_) = AbsSynResult23 (empty) ps0

generatedReduction51 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction51 ps0 ((AbsSynResult21 v1 ps1):_) = AbsSynResult24 (Just v1) ps1

generatedReduction52 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction52 ps0 ((AbsSynResult26 v2 ps2):(AbsSynResult28 v1 ps1):_) = AbsSynResult25 (PrecLevel v1 v2) ps1

generatedReduction53 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction53 ps0 ((AbsSynResult26 v2 ps2):(AbsSynResult27 v1 ps1):_) = AbsSynResult26 (v1:v2) ps1

generatedReduction54 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction54 ps0 ((AbsSynResult27 v1 ps1):_) = AbsSynResult26 ([v1]) ps1

generatedReduction55 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction55 ps0 ((AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult27 (v1) ps1

generatedReduction56 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction56 ps0 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult27 (v1) ps1

generatedReduction57 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction57 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult28 (LeftAssoc) ps1

generatedReduction58 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction58 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult28 (RightAssoc) ps1

generatedReduction59 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction59 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult28 (NonAssoc) ps1

generatedReduction60 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction60 ps0 ((AbsSynResult24 v4 ps4):(AbsSynResult30 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenUpperIdentifier v1))):_) = AbsSynResult29 (Rule v1 v3 v4) ps1

generatedReduction61 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction61 ps0 ((AbsSynResult23 v4 ps4):(AbsSynResult30 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenUpperIdentifier v1))):_) = AbsSynResult29 (Rule v1 v3 v4) ps1

generatedReduction62 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction62 ps0 ((AbsSynResult30 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult31 v1 ps1):_) = AbsSynResult30 (v1:v3) ps1

generatedReduction63 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction63 ps0 ((AbsSynResult31 v1 ps1):_) = AbsSynResult30 ([v1]) ps1

generatedReduction64 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction64 ps0 ((AbsSynToken (Token ps2 (TokenCustom ( TokenCodeBlock v2 )))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult31 (RuleProduction [] v2 Nothing) ps1

generatedReduction65 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction65 ps0 ((AbsSynToken (Token ps2 (TokenCustom ( TokenCodeBlock v2 )))):(AbsSynResult32 v1 ps1):_) = AbsSynResult31 (RuleProduction v1 v2 Nothing) ps1

generatedReduction66 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction66 ps0 ((AbsSynToken (Token ps4 (TokenCustom ( TokenCodeBlock v4 )))):(AbsSynToken (Token ps3 (TokenStringLit v3))):(AbsSynToken (Token ps2 v2)):(AbsSynResult32 v1 ps1):_) = AbsSynResult31 (RuleProduction v1 v4 $ Just v3) ps1

generatedReduction67 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction67 ps0 ((AbsSynToken (Token ps4 (TokenCustom ( TokenCodeBlock v4 )))):(AbsSynToken (Token ps3 (TokenIdentifier v3))):(AbsSynToken (Token ps2 v2)):(AbsSynResult32 v1 ps1):_) = AbsSynResult31 (RuleProduction v1 v4 $ Just v3) ps1

generatedReduction68 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction68 ps0 ((AbsSynResult32 v2 ps2):(AbsSynResult33 v1 ps1):_) = AbsSynResult32 (v1:v2) ps1

generatedReduction69 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction69 ps0 ((AbsSynResult33 v1 ps1):_) = AbsSynResult32 ([v1]) ps1

generatedReduction70 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction70 ps0 ((AbsSynResult37 v1 ps1):_) = AbsSynResult33 (RuleToken v1 RuleTokenModifierNormal) ps1

generatedReduction71 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction71 ps0 ((AbsSynResult35 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult37 v1 ps1):_) = AbsSynResult33 (RuleToken v1 $ RuleTokenModifierSome v3) ps1

generatedReduction72 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction72 ps0 ((AbsSynResult34 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult37 v1 ps1):_) = AbsSynResult33 (RuleToken v1 $ RuleTokenModifierSome v3) ps1

generatedReduction73 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction73 ps0 ((AbsSynResult35 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult37 v1 ps1):_) = AbsSynResult33 (RuleToken v1 $ RuleTokenModifierMany v3) ps1

generatedReduction74 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction74 ps0 ((AbsSynResult34 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult37 v1 ps1):_) = AbsSynResult33 (RuleToken v1 $ RuleTokenModifierMany v3) ps1

generatedReduction75 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction75 ps0 ((AbsSynToken (Token ps2 v2)):(AbsSynResult37 v1 ps1):_) = AbsSynResult33 (RuleToken v1 RuleTokenModifierOptional) ps1

generatedReduction76 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction76 ps0 (_) = AbsSynResult34 (empty) ps0

generatedReduction77 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction77 ps0 ((AbsSynResult36 v1 ps1):_) = AbsSynResult35 (Just v1) ps1

generatedReduction78 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction78 ps0 ((AbsSynToken (Token ps3 v3)):(AbsSynResult37 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult36 (v2) ps1

generatedReduction79 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction79 ps0 ((AbsSynToken (Token ps1 (TokenUpperIdentifier v1))):_) = AbsSynResult37 (RuleNonTerminal v1) ps1

generatedReduction80 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction80 ps0 ((AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult37 (RuleTerminal v1) ps1

generatedReduction81 :: ParseState -> [AbsSynToken] -> AbsSynToken
generatedReduction81 ps0 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult37 (RuleTerminal v1) ps1

