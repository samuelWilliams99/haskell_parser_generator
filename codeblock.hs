module CodeBlock where

import Grammar
import Data.Maybe
import Scanner
import Parsing

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
innerCodeBlock = fmap unpackCustom (codeBlock' True) <|> return ""

codeBlockAux :: Parser String
codeBlockAux = do
    preStr <- nonCurlyStr
    innerBlock <- innerCodeBlock
    let out = preStr ++ innerBlock
    if out == "" then empty else return out

withBraces :: Bool -> String -> String
withBraces True str = "{" ++ str ++ "}"
withBraces False str = str

codeBlock' :: Bool -> Parser TokenType
codeBlock' includeBraces = do
    char '{'
    str <- fmap concat $ many codeBlockAux
    postStr <- nonCurlyStr
    char '}'

    return $ TokenCustom "CodeBlock" $ withBraces includeBraces $ str ++ postStr

codeBlock :: Parser TokenType
codeBlock = codeBlock' False
