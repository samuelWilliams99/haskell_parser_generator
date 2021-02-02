{-|
Module      : Grammar
Description : Data structures and helper functions for a Grammar
Copyright   : (c) Samuel Williams, 2021
License     : GPL-3
Maintainer  : samuel.will1999@gmail.com
Stability   : release

This module holds the @Grammar@ data structure definition, used for generating the "DFA" structure, as well as various various getters for that structure.
It also holds the @ScannerSpec@ structure, which is an abstraction of "Scanner" used for generating the "Scanner" instance code.
-}
module Grammar where

import Data.HashMap.Strict hiding (map)
import Data.Hashable
import Data.List
import Data.Char

-- | Grammar interpreted from the gmr file, contains all information needed to generate a "DFA"
data Grammar = Grammar { tokenDefs :: [TokenDef] -- ^ All tokens definitions for a grammar
                       , precLevels :: [PrecLevel] -- ^ Precedence definitions for tokens
                       , rules :: [Rule] -- ^ Ordered rule base
                       } deriving Show

-- | Direct output from parser of token definitions, to be translated to a map.
data TokenDef = TokenDef { tokenName :: String -- ^ Name used in rules
                         , tokenPattern :: String -- ^ Haskell pattern to match in generated code
                         , tokenPatternType :: Maybe String -- ^ Optional haskell type of token
                         } deriving (Show)

-- | Hashing based only on name
instance Hashable TokenDef where
    hashWithSalt salt td = hashWithSalt salt $ tokenName td

-- | Equality based only on name
instance Eq TokenDef where
    a == b = (tokenName a) == (tokenName b)

-- | Parameterless data type for associativity
data Associativity = LeftAssoc | RightAssoc | NonAssoc deriving (Show, Eq)

-- | Precedence level with list of tokens and associativity for those tokens
data PrecLevel = PrecLevel { levelAssociativity :: Associativity
                           , levelTokens :: [String] -- ^ tokens as list of @TokenDef@ names
                           } deriving Show

-- | Grammar Rule as a list of productions for a non terminal
data Rule = Rule { ruleName :: String -- ^ Name of non terminal
                 , ruleProductions :: [RuleProduction]
                 , ruleResultType :: Maybe String
                 } deriving Show

-- | Equality based only the name
instance Eq Rule where
    a == b = (ruleName a) == (ruleName b)

-- | Tokens used indirectly in @RuleProduction@, as either terminals or non terminals
data RuleTokenType = RuleNonTerminal String | RuleTerminal String deriving (Show, Eq)

-- | Modifiers for @RuleToken@, eventually replaced with extra rules
data RuleTokenModifier = RuleTokenModifierNormal -- ^ Default rule behaviour, ignored on pre-processing
                       | RuleTokenModifierSome (Maybe RuleTokenType) -- ^ One or more matching for a token, with optional separator token
                       | RuleTokenModifierMany (Maybe RuleTokenType) -- ^ Zero or more matching for a token, with optional separator token
                       | RuleTokenModifierOptional -- ^ Zero or one matching for a token
                       deriving Show

-- | Token used within @RuleProduction@, holding a @RuleTokenType@ and associated @RuleTokenModifier@
data RuleToken = RuleToken { tokenType :: RuleTokenType
                           , tokenModifier :: RuleTokenModifier
                           } deriving Show

-- | Name getter for both types of @RuleTokenType@
getTokenTypeStr :: RuleTokenType -> String
getTokenTypeStr (RuleNonTerminal str) = str
getTokenTypeStr (RuleTerminal str) = str

-- | Name getter for @RuleToken@ wrapper
getTokenStr :: RuleToken -> String
getTokenStr = getTokenTypeStr . tokenType

-- | Creates a @RuleToken@ non terminal from its name
nonTerminalToken :: String -> RuleToken
nonTerminalToken str = ruleToken $ RuleNonTerminal str

-- | Creates a @RuleToken@ terminal from a @TokenDef@ name
terminalToken :: String -> RuleToken
terminalToken str = ruleToken $ RuleTerminal str

-- | Wraps a @RuleTokenType@ with the default @RuleTokenModifier@
ruleToken :: RuleTokenType -> RuleToken
ruleToken t = RuleToken t RuleTokenModifierNormal

-- | Grammar production, describing a rule of the form @A :: a B c   { haskellCode }@
data RuleProduction = RuleProduction { productionTokens :: [RuleToken] -- ^ Tokens of the production
                                     , productionResult :: String -- ^ Resulting haskell code for reducing this production
                                     , productionPrecToken :: (Maybe String) -- ^ Optional production precedence and associativity override
                                     } deriving Show

-- | Copy of the scanner object but all fields are an instance of Show (specifically changing the @ParserMap@ to a @String@)
data ScannerSpec = ScannerSpec
    { specSeparateCasedIdentifiers :: Bool
    , specIgnoreWhitespace :: Bool
    , specIgnoreComments :: Bool
    , specOperators :: [String]
    , specKeywords :: [String]
    , specBlockComment :: Maybe (String, String)
    , specLineComment :: Maybe String
    , specParserMap :: Maybe String
    } deriving Show

-- | Default definition for a ScannerSpec, for easy modification via record updating
scannerSpec = ScannerSpec
    { specSeparateCasedIdentifiers=False
    , specIgnoreWhitespace=True
    , specIgnoreComments=True
    , specOperators=[]
    , specKeywords=[]
    , specBlockComment=Nothing
    , specLineComment=Nothing
    , specParserMap=Nothing
    }

-- | Takes a @ScannerSpec@ and @Grammar@ and adds default and appropriate @TokenDef@s.
-- Adds defaults for all @Token@ constructors in "Scanner", generating those for operators and keywords.
addScannerSpecTokens :: ScannerSpec -> Grammar -> Grammar
addScannerSpecTokens spec gmr = gmr{ tokenDefs=nub $ tokenDefs gmr ++ makeTokens "TokenOperator " ops ++ makeTokens "TokenKeyword " kwds ++ defaultTokens }
  where
    ops = specOperators spec
    kwds = specKeywords spec
    makeTokens prefix = fmap $ \x -> TokenDef (map toLower x) (prefix ++ show x) (Just "String")
    defaultTokens = [ TokenDef "(" "TokenOpenParen" (Just "TokenType")
                    , TokenDef ")" "TokenCloseParen" (Just "TokenType")
                    , TokenDef "[" "TokenOpenSquare" (Just "TokenType")
                    , TokenDef "]" "TokenCloseSquare" (Just "TokenType")
                    , TokenDef "{" "TokenOpenCurly" (Just "TokenType")
                    , TokenDef "}" "TokenCloseCurly" (Just "TokenType")
                    , TokenDef "identifier" "TokenIdentifier $$" (Just "String")
                    , TokenDef "upperIdentifier" "TokenUpperIdentifier $$" (Just "String")
                    , TokenDef "stringLit" "TokenStringLit $$" (Just "String")
                    , TokenDef "integerLit" "TokenIntLit $$" (Just "Int")
                    , TokenDef "floatLit" "TokenFloatLit $$" (Just "Float")
                    , TokenDef "blockComment" "TokenBlockComment $$" (Just "String")
                    , TokenDef "lineComment" "TokenLineComment $$" (Just "String")
                    , TokenDef "whitespace" "TokenWhitespace $$" (Just "Char")]
