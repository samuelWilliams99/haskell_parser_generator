module Scanner where

import Parsing
import Result
import ParseState
import Data.Char
import Data.List
import Data.Foldable

data Scanner = Scanner
    { separateCasedIdentifiers :: Bool
    , ignoreWhitespace :: Bool
    , ignoreComments :: Bool
    , operators :: [String]
    , keywords :: [String]
    , blockComment :: Maybe (String, String)
    , lineComment :: Maybe String
    , includeEOF :: Bool
    , parserMap :: ([Parser TokenType] -> [Parser TokenType])
    }

-- Cannot show parserMap, so show every except that
instance Show Scanner where
    show (Scanner sep whsp cmts ops kwds block line eof _) =
        "Scanner {separateCasedIdentifiers = " ++ show sep ++
        ", ignoreWhitespace = " ++ show whsp ++
        ", ignoreComments = " ++ show cmts ++
        ", operators = " ++ show ops ++
        ", keywords = " ++ show kwds ++
        ", blockComment = " ++ show block ++
        ", lineComment = " ++ show line ++
        ", includeEOF = " ++ show eof ++ "}"

data Token = Token ParseState TokenType

-- Ignore parsestate when printing
instance Show Token where
    show (Token _ tt) = show tt

data TokenType = TokenIdentifier String
               | TokenUpperIdentifier String
               | TokenOperator String
               | TokenStringLit String
               | TokenIntLit Int
               | TokenFloatLit Float
               | TokenKeyword String
               | TokenWhitespace Char
               | TokenBlockComment String
               | TokenLineComment String
               | TokenCustom String String
               | TokenOpenParen  | TokenCloseParen
               | TokenOpenSquare | TokenCloseSquare
               | TokenOpenCurly  | TokenCloseCurly
               | TokenEOF
               deriving (Show)

-- Default scanner definition
scanner = Scanner{ separateCasedIdentifiers=True
                 , ignoreWhitespace=True
                 , ignoreComments=True
                 , operators=["+", "-", "*", "/", "="]
                 , keywords=["for", "if"]
                 , blockComment=Just ("/*", "*/")
                 , lineComment=Just "//"
                 , includeEOF=False
                 , parserMap=id
                 }

-- Parsers

space :: Parser TokenType
space = fmap TokenWhitespace $ sat isSpace

-- Packs token in with its ParseState
tokenWithState :: Parser TokenType -> Parser Token
tokenWithState p = do
    ps <- getState
    t <- p
    return $ Token ps t

-- Remove comment tokens if not enabled
asTokenComment :: Scanner -> Token -> [Token]
asTokenComment conf t@(Token _ (TokenBlockComment _)) = if ignoreComments conf then [] else [t]
asTokenComment conf t@(Token _ (TokenLineComment _))  = if ignoreComments conf then [] else [t]
asTokenComment conf t                                 = [t]

-- Find pre and post whitespaces, include in output if enabled
asToken :: Scanner -> Parser TokenType -> Parser [Token]
asToken conf p = do
    preSpaces <- many $ tokenWithState space
    token <- tokenWithState p
    postSpaces <- many $ tokenWithState space
    let tokens = asTokenComment conf token

    if ignoreWhitespace conf then
        return tokens
    else
        return $ preSpaces ++ tokens ++ postSpaces

-- Parser for string literals with escape characters such as "this is \"a\" test"
-- Replaces escaped quotes with their real value
-- TODO: deal with \n, currently just adds an n
formattedString :: Parser String
formattedString = do
    quote <- char '\'' <|> char '"'
    str <- many $ (char '\\' >> item)
                   <|> sat (/= quote)
    char quote

    return str

-- Version of formatted string that returns string unaltered
formattedStringRaw :: Parser String
formattedStringRaw = do
    quote <- char '\'' <|> char '"'
    str <- fmap concat $ many $
        fmap (\x -> ['\\', x]) (char '\\' >> item) <|>
        fmap pure (sat (/= quote))
    char quote

    return $ quote:str ++ [quote]

-- sort by length backwards
backLengthSorter = sortOn $ negate . length

-- anyString called with list of strings sorted longest to shortest, so partial matches don't cause issue
operator :: Scanner -> Parser TokenType
operator conf = fmap TokenOperator $ anyString $ backLengthSorter $ operators conf

keyword :: Scanner -> Parser TokenType
keyword conf = fmap TokenKeyword $ anyString $ backLengthSorter $ keywords conf

-- Match a specific character, return given token
getCharParser :: Char -> TokenType -> Parser TokenType
getCharParser c t = char c >> return t

-- Ensures next character is not alpha
nonAlphaNext :: Parser ()
nonAlphaNext = do
    next <- tryPeek
    case next of
        Just v  -> if isAlpha v then empty else return ()
        Nothing -> return ()

-- Pack parsers into TokenType parsers

stringToken :: Parser TokenType
stringToken = do
    v <- formattedString
    return $ TokenStringLit v

intToken :: Parser TokenType
intToken = do
    v <- int
    nonAlphaNext
    return $ TokenIntLit v

floatToken :: Parser TokenType
floatToken = do
    v <- float
    nonAlphaNext
    return $ TokenFloatLit v

lowerIdentifierToken :: Parser TokenType
lowerIdentifierToken = do
    id <- ident
    return $ TokenIdentifier id

upperIdentifierToken :: Parser TokenType
upperIdentifierToken = do
    id <- upperIdent
    return $ TokenUpperIdentifier id

identifierToken :: Parser TokenType
identifierToken = do
    id <- ident <|> upperIdent
    return $ TokenIdentifier id

brackets :: Parser TokenType
brackets = getCharParser '(' TokenOpenParen  <|>
           getCharParser ')' TokenCloseParen <|>
           getCharParser '{' TokenOpenCurly  <|>
           getCharParser '}' TokenCloseCurly <|>
           getCharParser '[' TokenOpenSquare <|>
           getCharParser ']' TokenCloseSquare

-- Return correct identifier parsers based on config
getIdentifierTokenParser :: Bool -> Parser TokenType
getIdentifierTokenParser isSeperate = if isSeperate then lowerIdentifierToken <|> upperIdentifierToken else identifierToken

-- Handle comments, cannot fail once openStr is read, else comment is ignored and error is wrong. non-close comments are thus allowed.
commentParser :: String -> String -> (String -> TokenType) -> Parser TokenType
commentParser openStr closeStr const = do
        string openStr
        commentBody <- many $ mustFail (string closeStr) >> item
        nextToken <- tryPeek
        if nextToken /= Nothing then string closeStr
        else return empty

        return $ const commentBody

blockCommentParser :: Scanner -> Parser TokenType
blockCommentParser conf = case blockComment conf of
    Nothing -> empty
    Just (openStr, closeStr) -> commentParser openStr closeStr TokenBlockComment

lineCommentParser :: Scanner -> Parser TokenType
lineCommentParser conf = case lineComment conf of
    Nothing -> empty
    Just str -> commentParser str "\n" TokenLineComment

-- The scanner itself
scanParser :: Scanner -> Parser [Token]
scanParser conf = fmap concat $ many $ asum tokenedParserList
  where
    parserList = [ stringToken
                 , blockCommentParser conf
                 , lineCommentParser conf
                 , operator conf
                 , brackets
                 , keyword conf
                 , getIdentifierTokenParser $ separateCasedIdentifiers conf
                 , intToken
                 , floatToken ]
    modifiedParserList = parserMap conf $ parserList
    tokenedParserList = map (asToken conf) modifiedParserList

-- Scan to a result, including EOF if enabled
scan :: Scanner -> String -> Result [Token]
scan conf str = do
    tokens <- parseToResult (scanParser conf) str
    if includeEOF conf then
        return $ tokens ++ [Token (fromPos str $ length str) TokenEOF]
    else
        return tokens
