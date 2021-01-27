module DFA where

import Grammar
import Data.HashMap.Strict
import Control.Lens

data DFA = DFA{ _dfaStates :: [DFAState]
              , _dfaProductions :: [DFAProduction]
              , _dfaTokenMap :: TokenMap
              , _dfaPrecMap :: PrecMap
              , _dfaFollowMap :: FollowMap
              } deriving Show

data Prec = Prec Associativity Int deriving Show

type PrecMap = HashMap TokenDef Prec

type TokenMap = HashMap String TokenDef

type FollowMap = HashMap String [TokenDef]

data DFAProduction = DFAProduction{ _dfaProductionName :: String
                                  , _dfaProductionTokens :: [RuleTokenType]
                                  , _dfaProductionResult :: String
                                  , _dfaProductionPrec :: (Maybe Prec)
                                  } deriving Show

data DFAAction = DFAShift Int | DFAReduce Int | DFAFinish deriving (Show, Eq)

-- Actions are a pair of token to read followed by above.
data DFAState = DFAState { productions :: [(Int, Int)]
                         , actions :: [(RuleTokenType, DFAAction)]
                         } deriving Show

-- No need to compare actions when comparing DFAStates, as actions are only dependent on productions, so if ps are the same, actions are (or will be) the same.
instance Eq DFAState where
    (DFAState ps _) == (DFAState ps' _) = ps == ps'

makeLenses ''DFA
makeLenses ''DFAProduction
