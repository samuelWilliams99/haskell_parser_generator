module Cfgparser (runParser, module Result) where

import Scanner
import Parsing
import Result
import ParseState
-- Need to modify the generic scanner to account for this kind of scanning, with weird tokens like %this and embedded code in another language { like this }

import Grammar
import Data.Maybe

unpackCustom :: TokenType -> String
unpackCustom (TokenCustom _ str) = str
unpackCustom _ = ""

nonCurly :: Parser String
nonCurly = do
    c <- item
    if (c == '}') || (c == '{') then
        empty
    else
        return [c]

nonCurlyStr :: Parser String
nonCurlyStr = fmap concat $ many $ formattedStringRaw <|> nonCurly

innerCodeBlock :: Parser String
innerCodeBlock = fmap unpackCustom (codeBlock True) <|> return ""

codeBlockAux :: Parser String
codeBlockAux = do
    preStr <- nonCurlyStr
    innerBlock <- innerCodeBlock
    let out = preStr ++ innerBlock
    if out == "" then empty else return out

withBraces :: Bool -> String -> String
withBraces True str = "{" ++ str ++ "}"
withBraces False str = str

codeBlock :: Bool -> Parser TokenType
codeBlock includeBraces = do
    char '{'
    str <- fmap concat $ many codeBlockAux
    postStr <- nonCurlyStr
    char '}'

    return $ TokenCustom "CodeBlock" $ withBraces includeBraces $ str ++ postStr

directive :: Parser TokenType
directive = do
    char '%'
    str <- ident
    return $ TokenCustom "Directive" str

extraParser :: Parser TokenType
extraParser = codeBlock False <|> directive

gScanner = Scanner{ separateCasedIdentifiers=True
                  , ignoreWhitespace=True
                  , ignoreComments=True
                  , operators=["::","|","*","+","?","=>"]
                  , keywords=[]
                  , lineComment=Just "#"
                  , blockComment=Just ("#[","]#")
                  , includeEOF=True
                  , parserMap=(extraParser:)
                  }

runParser str = do
    ts <- scan gScanner str
    generatedState0 [] [] $ fmap AbsSynToken ts

generatedError [] = Error "Ran out of tokens"
generatedError ((AbsSynToken (Token ps x)):xs) = Error $ "Unexpected token: " ++ (show x) ++ " at " ++ showPos ps

unpackFinal (AbsSynResult1 x) = x

generatedStates = [generatedState0, generatedState1, generatedState2, generatedState3, generatedState4, generatedState5, generatedState6, generatedState7, generatedState8, generatedState9, generatedState10, generatedState11, generatedState12, generatedState13, generatedState14, generatedState15, generatedState16, generatedState17, generatedState18, generatedState19, generatedState20, generatedState21, generatedState22, generatedState23, generatedState24, generatedState25, generatedState26, generatedState27, generatedState28, generatedState29, generatedState30, generatedState31, generatedState32, generatedState33, generatedState34, generatedState35, generatedState36, generatedState37, generatedState38, generatedState39, generatedState40, generatedState41, generatedState42, generatedState43, generatedState44, generatedState45, generatedState46, generatedState47, generatedState48, generatedState49, generatedState50, generatedState51, generatedState52, generatedState53, generatedState54, generatedState55, generatedState56, generatedState57, generatedState58, generatedState59, generatedState60, generatedState61, generatedState62, generatedState63, generatedState64, generatedState65, generatedState66, generatedState67, generatedState68, generatedState69, generatedState70, generatedState71, generatedState72, generatedState73, generatedState74, generatedState75, generatedState76, generatedState77, generatedState78, generatedState79, generatedState80, generatedState81, generatedState82, generatedState83, generatedState84, generatedState85, generatedState86, generatedState87, generatedState88, generatedState89, generatedState90, generatedState91, generatedState92, generatedState93, generatedState94, generatedState95, generatedState96, generatedState97, generatedState98, generatedState99, generatedState100, generatedState101, generatedState102, generatedState103]

data AbsSynToken t2 t3 t4 t5 t7 t8 t9 t10 t11 t13 t15 t18 t19 t21 t25 t27 t29 t30 = AbsSynToken Token | AbsSynResult1 ( String, [(String, [String])], Grammar ) | AbsSynResult2 t2 | AbsSynResult3 t3 | AbsSynResult4 t4 | AbsSynResult5 t5 | AbsSynResult6 Grammar | AbsSynResult7 t7 | AbsSynResult8 t8 | AbsSynResult9 t9 | AbsSynResult10 t10 | AbsSynResult11 t11 | AbsSynResult12 (String, [String]) | AbsSynResult13 t13 | AbsSynResult14 [TokenDef] | AbsSynResult15 t15 | AbsSynResult16 String | AbsSynResult17 TokenDef | AbsSynResult18 t18 | AbsSynResult19 t19 | AbsSynResult20 PrecLevel | AbsSynResult21 t21 | AbsSynResult22 String | AbsSynResult23 Associativity | AbsSynResult24 Rule | AbsSynResult25 t25 | AbsSynResult26 RuleProduction | AbsSynResult27 t27 | AbsSynResult28 RuleToken | AbsSynResult29 t29 | AbsSynResult30 t30 | AbsSynResult31 RuleTokenType | AbsSynResult32 RuleTokenType

generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = generatedState0 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState1 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynResult2 _):xs) = generatedState2 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynResult3 _):xs) = generatedState98 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynResult1 _):xs) = generatedState103 (x:vs) (0:ss) xs
generatedState0 vs ss xs = generatedError xs


generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState1 vs ss xs = generatedError xs


generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = generatedState3 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = generatedState5 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = generatedState6 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = generatedState7 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = generatedState8 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = generatedState11 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = generatedState13 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = generatedState17 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = generatedState2 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState2 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState2 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState2 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState2 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState2 vs ss (x@(AbsSynResult12 _):xs) = generatedState19 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynResult4 _):xs) = generatedState21 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynResult5 _):xs) = generatedState96 (x:vs) (2:ss) xs
generatedState2 vs ss xs = generatedError xs


generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState4 (x:vs) (3:ss) xs
generatedState3 vs ss xs = generatedError xs


generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState4 vs ss xs = generatedError xs


generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState5 vs ss xs = generatedError xs


generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState6 vs ss xs = generatedError xs


generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState7 vs ss xs = generatedError xs


generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState9 (x:vs) (8:ss) xs
generatedState8 vs ss xs = generatedError xs


generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState10 (x:vs) (9:ss) xs
generatedState9 vs ss xs = generatedError xs


generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState10 vs ss xs = generatedError xs


generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState12 (x:vs) (11:ss) xs
generatedState11 vs ss xs = generatedError xs


generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState12 vs ss xs = generatedError xs


generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState14 (x:vs) (13:ss) xs
generatedState13 vs ss (x@(AbsSynResult13 _):xs) = generatedState16 (x:vs) (13:ss) xs
generatedState13 vs ss xs = generatedError xs


generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState14 (x:vs) (14:ss) xs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState14 vs ss (x@(AbsSynResult13 _):xs) = generatedState15 (x:vs) (14:ss) xs
generatedState14 vs ss xs = generatedError xs


generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState15 vs ss xs = generatedError xs


generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState16 vs ss xs = generatedError xs


generatedState17 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState14 (x:vs) (17:ss) xs
generatedState17 vs ss (x@(AbsSynResult13 _):xs) = generatedState18 (x:vs) (17:ss) xs
generatedState17 vs ss xs = generatedError xs


generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState18 vs ss xs = generatedError xs


generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = generatedState3 (x:vs) (19:ss) xs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = generatedState5 (x:vs) (19:ss) xs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = generatedState6 (x:vs) (19:ss) xs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = generatedState7 (x:vs) (19:ss) xs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = generatedState8 (x:vs) (19:ss) xs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = generatedState11 (x:vs) (19:ss) xs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = generatedState13 (x:vs) (19:ss) xs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = generatedState17 (x:vs) (19:ss) xs
generatedState19 vs ss (x@(AbsSynResult12 _):xs) = generatedState19 (x:vs) (19:ss) xs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState19 vs ss (x@(AbsSynResult5 _):xs) = generatedState20 (x:vs) (19:ss) xs
generatedState19 vs ss xs = generatedError xs


generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction8 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction8 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction8 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction8 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction8 vs
generatedState20 vs ss xs = generatedError xs


generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = generatedState22 (x:vs) (21:ss) xs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState21 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState21 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState21 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState21 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState21 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState21 vs ss (x@(AbsSynResult14 _):xs) = generatedState37 (x:vs) (21:ss) xs
generatedState21 vs ss (x@(AbsSynResult7 _):xs) = generatedState38 (x:vs) (21:ss) xs
generatedState21 vs ss (x@(AbsSynResult8 _):xs) = generatedState90 (x:vs) (21:ss) xs
generatedState21 vs ss (x@(AbsSynResult6 _):xs) = generatedState95 (x:vs) (21:ss) xs
generatedState21 vs ss xs = generatedError xs


generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState23 (x:vs) (22:ss) xs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState30 (x:vs) (22:ss) xs
generatedState22 vs ss (x@(AbsSynResult17 _):xs) = generatedState34 (x:vs) (22:ss) xs
generatedState22 vs ss (x@(AbsSynResult15 _):xs) = generatedState36 (x:vs) (22:ss) xs
generatedState22 vs ss xs = generatedError xs


generatedState23 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState24 (x:vs) (23:ss) xs
generatedState23 vs ss xs = generatedError xs


generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = generatedState25 (x:vs) (24:ss) xs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState24 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState24 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState24 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState24 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState24 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState24 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState24 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState24 vs ss (x@(AbsSynResult16 _):xs) = generatedState27 (x:vs) (24:ss) xs
generatedState24 vs ss (x@(AbsSynResult18 _):xs) = generatedState28 (x:vs) (24:ss) xs
generatedState24 vs ss (x@(AbsSynResult19 _):xs) = generatedState29 (x:vs) (24:ss) xs
generatedState24 vs ss xs = generatedError xs


generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState26 (x:vs) (25:ss) xs
generatedState25 vs ss xs = generatedError xs


generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState26 vs ss xs = generatedError xs


generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState27 vs ss xs = generatedError xs


generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState28 vs ss xs = generatedError xs


generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState29 vs ss xs = generatedError xs


generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState31 (x:vs) (30:ss) xs
generatedState30 vs ss xs = generatedError xs


generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = generatedState25 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState31 vs ss (x@(AbsSynResult16 _):xs) = generatedState27 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynResult18 _):xs) = generatedState32 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynResult19 _):xs) = generatedState33 (x:vs) (31:ss) xs
generatedState31 vs ss xs = generatedError xs


generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState32 vs ss xs = generatedError xs


generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState33 vs ss xs = generatedError xs


generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState23 (x:vs) (34:ss) xs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState30 (x:vs) (34:ss) xs
generatedState34 vs ss (x@(AbsSynResult17 _):xs) = generatedState34 (x:vs) (34:ss) xs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState34 vs ss (x@(AbsSynResult15 _):xs) = generatedState35 (x:vs) (34:ss) xs
generatedState34 vs ss xs = generatedError xs


generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState35 vs ss xs = generatedError xs


generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState36 vs ss xs = generatedError xs


generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState37 vs ss xs = generatedError xs


generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState39 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState40 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState41 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynResult23 _):xs) = generatedState42 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState38 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState38 vs ss (x@(AbsSynResult20 _):xs) = generatedState48 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynResult9 _):xs) = generatedState50 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynResult10 _):xs) = generatedState88 (x:vs) (38:ss) xs
generatedState38 vs ss xs = generatedError xs


generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction48 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction48 vs
generatedState39 vs ss xs = generatedError xs


generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState40 vs ss xs = generatedError xs


generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction46 vs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction46 vs
generatedState41 vs ss xs = generatedError xs


generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState43 (x:vs) (42:ss) xs
generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState44 (x:vs) (42:ss) xs
generatedState42 vs ss (x@(AbsSynResult22 _):xs) = generatedState45 (x:vs) (42:ss) xs
generatedState42 vs ss (x@(AbsSynResult21 _):xs) = generatedState47 (x:vs) (42:ss) xs
generatedState42 vs ss xs = generatedError xs


generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState43 vs ss xs = generatedError xs


generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState44 vs ss xs = generatedError xs


generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState43 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState44 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynResult22 _):xs) = generatedState45 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState45 vs ss (x@(AbsSynResult21 _):xs) = generatedState46 (x:vs) (45:ss) xs
generatedState45 vs ss xs = generatedError xs


generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss xs = generatedError xs


generatedState47 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState47 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState47 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState47 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState47 vs ss xs = generatedError xs


generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState39 (x:vs) (48:ss) xs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState40 (x:vs) (48:ss) xs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState41 (x:vs) (48:ss) xs
generatedState48 vs ss (x@(AbsSynResult23 _):xs) = generatedState42 (x:vs) (48:ss) xs
generatedState48 vs ss (x@(AbsSynResult20 _):xs) = generatedState48 (x:vs) (48:ss) xs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState48 vs ss (x@(AbsSynResult10 _):xs) = generatedState49 (x:vs) (48:ss) xs
generatedState48 vs ss xs = generatedError xs


generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState49 vs ss xs = generatedError xs


generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState51 (x:vs) (50:ss) xs
generatedState50 vs ss (x@(AbsSynResult24 _):xs) = generatedState85 (x:vs) (50:ss) xs
generatedState50 vs ss (x@(AbsSynResult11 _):xs) = generatedState87 (x:vs) (50:ss) xs
generatedState50 vs ss xs = generatedError xs


generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenOperator "::"))):xs) = generatedState52 (x:vs) (51:ss) xs
generatedState51 vs ss xs = generatedError xs


generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState53 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState54 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState55 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynResult32 _):xs) = generatedState56 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynResult28 _):xs) = generatedState68 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynResult27 _):xs) = generatedState70 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "empty"))):xs) = generatedState77 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynResult26 _):xs) = generatedState79 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynResult25 _):xs) = generatedState82 (x:vs) (52:ss) xs
generatedState52 vs ss xs = generatedError xs


generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenOperator "?"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState53 vs ss xs = generatedError xs


generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenOperator "?"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState54 vs ss xs = generatedError xs


generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "?"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState55 vs ss xs = generatedError xs


generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenOperator "?"))):xs) = generatedState57 (x:vs) (56:ss) xs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState58 (x:vs) (56:ss) xs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState65 (x:vs) (56:ss) xs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState56 vs ss xs = generatedError xs


generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction64 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction64 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction64 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction64 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction64 vs
generatedState57 vs ss xs = generatedError xs


generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState59 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = generatedState58 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState58 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState58 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState58 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState58 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState58 vs ss (x@(AbsSynResult31 _):xs) = generatedState62 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynResult29 _):xs) = generatedState63 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynResult30 _):xs) = generatedState64 (x:vs) (58:ss) xs
generatedState58 vs ss xs = generatedError xs


generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState53 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState54 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState55 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynResult32 _):xs) = generatedState60 (x:vs) (59:ss) xs
generatedState59 vs ss xs = generatedError xs


generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = generatedState61 (x:vs) (60:ss) xs
generatedState60 vs ss xs = generatedError xs


generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction67 vs
generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction67 vs
generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction67 vs
generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction67 vs
generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction67 vs
generatedState61 vs ss xs = generatedError xs


generatedState62 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 vs
generatedState62 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 vs
generatedState62 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 vs
generatedState62 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 vs
generatedState62 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 vs
generatedState62 vs ss xs = generatedError xs


generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction63 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction63 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction63 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction63 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction63 vs
generatedState63 vs ss xs = generatedError xs


generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState64 vs ss xs = generatedError xs


generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState59 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = generatedState65 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState65 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState65 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState65 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState65 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState65 vs ss (x@(AbsSynResult31 _):xs) = generatedState62 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynResult29 _):xs) = generatedState66 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynResult30 _):xs) = generatedState67 (x:vs) (65:ss) xs
generatedState65 vs ss xs = generatedError xs


generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState66 vs ss xs = generatedError xs


generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState67 vs ss xs = generatedError xs


generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState53 (x:vs) (68:ss) xs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState54 (x:vs) (68:ss) xs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState55 (x:vs) (68:ss) xs
generatedState68 vs ss (x@(AbsSynResult32 _):xs) = generatedState56 (x:vs) (68:ss) xs
generatedState68 vs ss (x@(AbsSynResult28 _):xs) = generatedState68 (x:vs) (68:ss) xs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction58 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction58 vs
generatedState68 vs ss (x@(AbsSynResult27 _):xs) = generatedState69 (x:vs) (68:ss) xs
generatedState68 vs ss xs = generatedError xs


generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction57 vs
generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction57 vs
generatedState69 vs ss xs = generatedError xs


generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "prec"))):xs) = generatedState71 (x:vs) (70:ss) xs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState76 (x:vs) (70:ss) xs
generatedState70 vs ss xs = generatedError xs


generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState72 (x:vs) (71:ss) xs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState74 (x:vs) (71:ss) xs
generatedState71 vs ss xs = generatedError xs


generatedState72 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState73 (x:vs) (72:ss) xs
generatedState72 vs ss xs = generatedError xs


generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction56 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction56 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction56 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenOperator "|"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction56 vs
generatedState73 vs ss xs = generatedError xs


generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState75 (x:vs) (74:ss) xs
generatedState74 vs ss xs = generatedError xs


generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction55 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction55 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction55 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenOperator "|"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction55 vs
generatedState75 vs ss xs = generatedError xs


generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenOperator "|"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState76 vs ss xs = generatedError xs


generatedState77 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState78 (x:vs) (77:ss) xs
generatedState77 vs ss xs = generatedError xs


generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenOperator "|"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 vs
generatedState78 vs ss xs = generatedError xs


generatedState79 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState79 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState79 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState79 vs ss (x@(AbsSynToken (Token ps (TokenOperator "|"))):xs) = generatedState80 (x:vs) (79:ss) xs
generatedState79 vs ss xs = generatedError xs


generatedState80 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState53 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState54 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState55 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynResult32 _):xs) = generatedState56 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynResult28 _):xs) = generatedState68 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynResult27 _):xs) = generatedState70 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "empty"))):xs) = generatedState77 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynResult26 _):xs) = generatedState79 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynResult25 _):xs) = generatedState81 (x:vs) (80:ss) xs
generatedState80 vs ss xs = generatedError xs


generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState81 vs ss xs = generatedError xs


generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = generatedState25 (x:vs) (82:ss) xs
generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState82 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState82 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState82 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState82 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState82 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState82 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState82 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState82 vs ss (x@(AbsSynResult16 _):xs) = generatedState27 (x:vs) (82:ss) xs
generatedState82 vs ss (x@(AbsSynResult18 _):xs) = generatedState83 (x:vs) (82:ss) xs
generatedState82 vs ss (x@(AbsSynResult19 _):xs) = generatedState84 (x:vs) (82:ss) xs
generatedState82 vs ss xs = generatedError xs


generatedState83 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState83 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState83 vs ss xs = generatedError xs


generatedState84 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState84 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState84 vs ss xs = generatedError xs


generatedState85 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState51 (x:vs) (85:ss) xs
generatedState85 vs ss (x@(AbsSynResult24 _):xs) = generatedState85 (x:vs) (85:ss) xs
generatedState85 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState85 vs ss (x@(AbsSynResult11 _):xs) = generatedState86 (x:vs) (85:ss) xs
generatedState85 vs ss xs = generatedError xs


generatedState86 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState86 vs ss xs = generatedError xs


generatedState87 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction13 vs
generatedState87 vs ss xs = generatedError xs


generatedState88 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState51 (x:vs) (88:ss) xs
generatedState88 vs ss (x@(AbsSynResult24 _):xs) = generatedState85 (x:vs) (88:ss) xs
generatedState88 vs ss (x@(AbsSynResult11 _):xs) = generatedState89 (x:vs) (88:ss) xs
generatedState88 vs ss xs = generatedError xs


generatedState89 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction12 vs
generatedState89 vs ss xs = generatedError xs


generatedState90 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState39 (x:vs) (90:ss) xs
generatedState90 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState40 (x:vs) (90:ss) xs
generatedState90 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState41 (x:vs) (90:ss) xs
generatedState90 vs ss (x@(AbsSynResult23 _):xs) = generatedState42 (x:vs) (90:ss) xs
generatedState90 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState90 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState90 vs ss (x@(AbsSynResult20 _):xs) = generatedState48 (x:vs) (90:ss) xs
generatedState90 vs ss (x@(AbsSynResult9 _):xs) = generatedState91 (x:vs) (90:ss) xs
generatedState90 vs ss (x@(AbsSynResult10 _):xs) = generatedState93 (x:vs) (90:ss) xs
generatedState90 vs ss xs = generatedError xs


generatedState91 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState51 (x:vs) (91:ss) xs
generatedState91 vs ss (x@(AbsSynResult24 _):xs) = generatedState85 (x:vs) (91:ss) xs
generatedState91 vs ss (x@(AbsSynResult11 _):xs) = generatedState92 (x:vs) (91:ss) xs
generatedState91 vs ss xs = generatedError xs


generatedState92 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction11 vs
generatedState92 vs ss xs = generatedError xs


generatedState93 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState51 (x:vs) (93:ss) xs
generatedState93 vs ss (x@(AbsSynResult24 _):xs) = generatedState85 (x:vs) (93:ss) xs
generatedState93 vs ss (x@(AbsSynResult11 _):xs) = generatedState94 (x:vs) (93:ss) xs
generatedState93 vs ss xs = generatedError xs


generatedState94 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction10 vs
generatedState94 vs ss xs = generatedError xs


generatedState95 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction4 vs
generatedState95 vs ss xs = generatedError xs


generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = generatedState22 (x:vs) (96:ss) xs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState96 vs ss (x@(AbsSynResult14 _):xs) = generatedState37 (x:vs) (96:ss) xs
generatedState96 vs ss (x@(AbsSynResult7 _):xs) = generatedState38 (x:vs) (96:ss) xs
generatedState96 vs ss (x@(AbsSynResult8 _):xs) = generatedState90 (x:vs) (96:ss) xs
generatedState96 vs ss (x@(AbsSynResult6 _):xs) = generatedState97 (x:vs) (96:ss) xs
generatedState96 vs ss xs = generatedError xs


generatedState97 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction3 vs
generatedState97 vs ss xs = generatedError xs


generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "parsermap"))):xs) = generatedState3 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepcomments"))):xs) = generatedState5 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keepwhitespaces"))):xs) = generatedState6 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "separateidentitycase"))):xs) = generatedState7 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "blockcomments"))):xs) = generatedState8 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "linecomments"))):xs) = generatedState11 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "keywords"))):xs) = generatedState13 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "operators"))):xs) = generatedState17 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = generatedState98 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState98 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState98 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState98 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState98 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState98 vs ss (x@(AbsSynResult12 _):xs) = generatedState19 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynResult4 _):xs) = generatedState99 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynResult5 _):xs) = generatedState101 (x:vs) (98:ss) xs
generatedState98 vs ss xs = generatedError xs


generatedState99 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = generatedState22 (x:vs) (99:ss) xs
generatedState99 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState99 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState99 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState99 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState99 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState99 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState99 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState99 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState99 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState99 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState99 vs ss (x@(AbsSynResult14 _):xs) = generatedState37 (x:vs) (99:ss) xs
generatedState99 vs ss (x@(AbsSynResult7 _):xs) = generatedState38 (x:vs) (99:ss) xs
generatedState99 vs ss (x@(AbsSynResult8 _):xs) = generatedState90 (x:vs) (99:ss) xs
generatedState99 vs ss (x@(AbsSynResult6 _):xs) = generatedState100 (x:vs) (99:ss) xs
generatedState99 vs ss xs = generatedError xs


generatedState100 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction2 vs
generatedState100 vs ss xs = generatedError xs


generatedState101 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "token"))):xs) = generatedState22 (x:vs) (101:ss) xs
generatedState101 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState101 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState101 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState101 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState101 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "left"))):xs) = generatedState101 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState101 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "right"))):xs) = generatedState101 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState101 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "nonassoc"))):xs) = generatedState101 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState101 vs ss (x@(AbsSynResult14 _):xs) = generatedState37 (x:vs) (101:ss) xs
generatedState101 vs ss (x@(AbsSynResult7 _):xs) = generatedState38 (x:vs) (101:ss) xs
generatedState101 vs ss (x@(AbsSynResult8 _):xs) = generatedState90 (x:vs) (101:ss) xs
generatedState101 vs ss (x@(AbsSynResult6 _):xs) = generatedState102 (x:vs) (101:ss) xs
generatedState101 vs ss xs = generatedError xs


generatedState102 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction1 vs
generatedState102 vs ss xs = generatedError xs


generatedState103 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = return $ unpackFinal $ head vs
generatedState103 vs ss xs = generatedError xs


generatedReduction1 ((AbsSynResult6 v3):(AbsSynResult5 v2):(AbsSynResult3 v1):_) = AbsSynResult1 ((fromMaybe "" v1, v2, v3))

generatedReduction2 ((AbsSynResult6 v3):(AbsSynResult4 v2):(AbsSynResult3 v1):_) = AbsSynResult1 ((fromMaybe "" v1, v2, v3))

generatedReduction3 ((AbsSynResult6 v3):(AbsSynResult5 v2):(AbsSynResult2 v1):_) = AbsSynResult1 ((fromMaybe "" v1, v2, v3))

generatedReduction4 ((AbsSynResult6 v3):(AbsSynResult4 v2):(AbsSynResult2 v1):_) = AbsSynResult1 ((fromMaybe "" v1, v2, v3))

generatedReduction5 (_) = AbsSynResult2 (empty)

generatedReduction6 ((AbsSynToken (Token ps1 (TokenCustom "CodeBlock" v1))):_) = AbsSynResult3 (Just v1)

generatedReduction7 (_) = AbsSynResult4 (empty)

generatedReduction8 ((AbsSynResult5 v2):(AbsSynResult12 v1):_) = AbsSynResult5 (v1:v2)

generatedReduction9 ((AbsSynResult12 v1):_) = AbsSynResult5 ([v1])

generatedReduction10 ((AbsSynResult11 v3):(AbsSynResult10 v2):(AbsSynResult8 v1):_) = AbsSynResult6 (Grammar (fromMaybe [] v1) v2 v3)

generatedReduction11 ((AbsSynResult11 v3):(AbsSynResult9 v2):(AbsSynResult8 v1):_) = AbsSynResult6 (Grammar (fromMaybe [] v1) v2 v3)

generatedReduction12 ((AbsSynResult11 v3):(AbsSynResult10 v2):(AbsSynResult7 v1):_) = AbsSynResult6 (Grammar (fromMaybe [] v1) v2 v3)

generatedReduction13 ((AbsSynResult11 v3):(AbsSynResult9 v2):(AbsSynResult7 v1):_) = AbsSynResult6 (Grammar (fromMaybe [] v1) v2 v3)

generatedReduction14 (_) = AbsSynResult7 (empty)

generatedReduction15 ((AbsSynResult14 v1):_) = AbsSynResult8 (Just v1)

generatedReduction16 (_) = AbsSynResult9 (empty)

generatedReduction17 ((AbsSynResult10 v2):(AbsSynResult20 v1):_) = AbsSynResult10 (v1:v2)

generatedReduction18 ((AbsSynResult20 v1):_) = AbsSynResult10 ([v1])

generatedReduction19 ((AbsSynResult11 v2):(AbsSynResult24 v1):_) = AbsSynResult11 (v1:v2)

generatedReduction20 ((AbsSynResult24 v1):_) = AbsSynResult11 ([v1])

generatedReduction21 ((AbsSynResult13 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (("ops", v2))

generatedReduction22 ((AbsSynResult13 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (("kwds", v2))

generatedReduction23 ((AbsSynToken (Token ps2 (TokenStringLit v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (("line", [v2]))

generatedReduction24 ((AbsSynToken (Token ps3 (TokenStringLit v3))):(AbsSynToken (Token ps2 (TokenStringLit v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (("block", [v2, v3]))

generatedReduction25 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (("sepiden", []))

generatedReduction26 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (("keepspace", []))

generatedReduction27 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (("keepcmts", []))

generatedReduction28 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (("parser", [v2]))

generatedReduction29 ((AbsSynResult13 v2):(AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult13 (v1:v2)

generatedReduction30 ((AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult13 ([v1])

generatedReduction31 ((AbsSynResult15 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult14 (v2)

generatedReduction32 ((AbsSynResult15 v2):(AbsSynResult17 v1):_) = AbsSynResult15 (v1:v2)

generatedReduction33 ((AbsSynResult17 v1):_) = AbsSynResult15 ([v1])

generatedReduction34 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult16 (v2)

generatedReduction35 ((AbsSynResult19 v3):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult17 (TokenDef v1 v2 v3)

generatedReduction36 ((AbsSynResult18 v3):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult17 (TokenDef v1 v2 v3)

generatedReduction37 ((AbsSynResult19 v3):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult17 (TokenDef v1 v2 v3)

generatedReduction38 ((AbsSynResult18 v3):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult17 (TokenDef v1 v2 v3)

generatedReduction39 (_) = AbsSynResult18 (empty)

generatedReduction40 ((AbsSynResult16 v1):_) = AbsSynResult19 (Just v1)

generatedReduction41 ((AbsSynResult21 v2):(AbsSynResult23 v1):_) = AbsSynResult20 (PrecLevel v1 v2)

generatedReduction42 ((AbsSynResult21 v2):(AbsSynResult22 v1):_) = AbsSynResult21 (v1:v2)

generatedReduction43 ((AbsSynResult22 v1):_) = AbsSynResult21 ([v1])

generatedReduction44 ((AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult22 (v1)

generatedReduction45 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult22 (v1)

generatedReduction46 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult23 (LeftAssoc)

generatedReduction47 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult23 (RightAssoc)

generatedReduction48 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult23 (NonAssoc)

generatedReduction49 ((AbsSynResult19 v4):(AbsSynResult25 v3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenUpperIdentifier v1))):_) = AbsSynResult24 (Rule v1 v3 v4)

generatedReduction50 ((AbsSynResult18 v4):(AbsSynResult25 v3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenUpperIdentifier v1))):_) = AbsSynResult24 (Rule v1 v3 v4)

generatedReduction51 ((AbsSynResult25 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult26 v1):_) = AbsSynResult25 (v1:v3)

generatedReduction52 ((AbsSynResult26 v1):_) = AbsSynResult25 ([v1])

generatedReduction53 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult26 (RuleProduction [] v2 Nothing)

generatedReduction54 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynResult27 v1):_) = AbsSynResult26 (RuleProduction v1 v2 Nothing)

generatedReduction55 ((AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 (TokenStringLit v3))):(AbsSynToken (Token ps2 v2)):(AbsSynResult27 v1):_) = AbsSynResult26 (RuleProduction v1 v4 $ Just v3)

generatedReduction56 ((AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 (TokenIdentifier v3))):(AbsSynToken (Token ps2 v2)):(AbsSynResult27 v1):_) = AbsSynResult26 (RuleProduction v1 v4 $ Just v3)

generatedReduction57 ((AbsSynResult27 v2):(AbsSynResult28 v1):_) = AbsSynResult27 (v1:v2)

generatedReduction58 ((AbsSynResult28 v1):_) = AbsSynResult27 ([v1])

generatedReduction59 ((AbsSynResult32 v1):_) = AbsSynResult28 (RuleToken v1 RuleTokenModifierNormal)

generatedReduction60 ((AbsSynResult30 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult32 v1):_) = AbsSynResult28 (RuleToken v1 $ RuleTokenModifierSome v3)

generatedReduction61 ((AbsSynResult29 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult32 v1):_) = AbsSynResult28 (RuleToken v1 $ RuleTokenModifierSome v3)

generatedReduction62 ((AbsSynResult30 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult32 v1):_) = AbsSynResult28 (RuleToken v1 $ RuleTokenModifierMany v3)

generatedReduction63 ((AbsSynResult29 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult32 v1):_) = AbsSynResult28 (RuleToken v1 $ RuleTokenModifierMany v3)

generatedReduction64 ((AbsSynToken (Token ps2 v2)):(AbsSynResult32 v1):_) = AbsSynResult28 (RuleToken v1 RuleTokenModifierOptional)

generatedReduction65 (_) = AbsSynResult29 (empty)

generatedReduction66 ((AbsSynResult31 v1):_) = AbsSynResult30 (Just v1)

generatedReduction67 ((AbsSynToken (Token ps3 v3)):(AbsSynResult32 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult31 (v2)

generatedReduction68 ((AbsSynToken (Token ps1 (TokenUpperIdentifier v1))):_) = AbsSynResult32 (RuleNonTerminal v1)

generatedReduction69 ((AbsSynToken (Token ps1 (TokenStringLit v1))):_) = AbsSynResult32 (RuleTerminal v1)

generatedReduction70 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult32 (RuleTerminal v1)

