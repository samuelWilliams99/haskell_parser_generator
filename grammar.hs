module Grammar where

import Data.HashMap.Strict
import Data.Hashable
import Data.List

data Grammar = Grammar { tokenDefs :: [TokenDef]
                       , precLevels :: [PrecLevel]
                       , rules :: [Rule]
                       } deriving Show

data TokenDef = TokenDef { tokenName :: String
                         , tokenPattern :: String
                         } deriving (Show)

-- To use TokenDef as a key, it must be hashable. We can easily avoid multiple token defs by making the name the key
-- To hash, we simply hash the tokendef name
instance Hashable TokenDef where
    hashWithSalt salt (TokenDef name _) = hashWithSalt salt name

-- Similar to above, equality need only check the name
instance Eq TokenDef where
    (TokenDef a _) == (TokenDef b _) = a == b

data Associativity = LeftAssoc | RightAssoc | NonAssoc deriving (Show, Eq)

data PrecLevel = PrecLevel { levelAssociativity :: Associativity
                           , levelTokens :: [String]
                           } deriving Show

data Rule = Rule { ruleName :: String
                 , ruleProductions :: [RuleProduction]
                 } deriving Show

-- Rule equality also need only check the name, easy way to ensure uniqueness by name
instance Eq Rule where
    a == b = (ruleName a) == (ruleName b)

data RuleTokenType = RuleNonTerminal String | RuleTerminal String deriving (Show, Eq)

data RuleTokenModifier = RuleTokenModifierNormal
                       | RuleTokenModifierSome (Maybe RuleTokenType)
                       | RuleTokenModifierMany (Maybe RuleTokenType)
                       | RuleTokenModifierOptional
                       deriving Show

data RuleToken = RuleToken { tokenType :: RuleTokenType
                           , tokenModifier :: RuleTokenModifier
                           } deriving Show

getTokenTypeStr :: RuleTokenType -> String
getTokenTypeStr (RuleNonTerminal str) = str
getTokenTypeStr (RuleTerminal str) = str

getTokenStr :: RuleToken -> String
getTokenStr = getTokenTypeStr . tokenType

-- Simple token builders
nonTerminalToken :: String -> RuleToken
nonTerminalToken str = ruleToken $ RuleNonTerminal str

terminalToken :: String -> RuleToken
terminalToken str = ruleToken $ RuleTerminal str

ruleToken :: RuleTokenType -> RuleToken
ruleToken t = RuleToken t RuleTokenModifierNormal

data RuleProduction = RuleProduction { productionTokens :: [RuleToken]
                                     , productionResult :: String
                                     , productionPrecToken :: (Maybe String)
                                     } deriving Show

-- Copy of the scanner object but where all fields are an instance of Show (specifically changing the ParserMap to a String)
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

-- Default args for a ScannerSpec
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

addScannerSpecTokens :: ScannerSpec -> Grammar -> Grammar
addScannerSpecTokens spec gmr = gmr{ tokenDefs=nub $ tokenDefs gmr ++ makeTokens "TokenOperator " ops ++ makeTokens "TokenKeyword " kwds ++ defaultTokens }
  where
    ops = specOperators spec
    kwds = specKeywords spec
    makeTokens prefix = fmap $ \x -> TokenDef x $ prefix ++ show x
    defaultTokens = [ TokenDef "(" "TokenOpenParen"
                    , TokenDef ")" "TokenCloseParen"
                    , TokenDef "[" "TokenOpenSquare"
                    , TokenDef "]" "TokenCloseSquare"
                    , TokenDef "{" "TokenOpenCurly"
                    , TokenDef "}" "TokenCloseCurly"
                    , TokenDef "identifier" "TokenIdentifier $$"
                    , TokenDef "upperIdentifier" "TokenUpperIdentifier $$"
                    , TokenDef "stringLit" "TokenStringLit $$"
                    , TokenDef "integerLit" "TokenIntLit $$"
                    , TokenDef "floatLit" "TokenFloatLit $$"
                    , TokenDef "blockComment" "TokenBlockComment $$"
                    , TokenDef "lineComment" "TokenLineComment $$"
                    , TokenDef "whitespace" "TokenWhitespace $$" ]
