{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : DFA
Description : Deterministic Finite Automata (or DFA) structure outputted from "ShiftReduce"
Copyright   : (c) Samuel Williams, 2021
License     : GPL-3
Maintainer  : samuel.will1999@gmail.com
Stability   : release

The DFA holds a list of @DFAStates@, each of which hold links to other states on reading token, similarly to how a Turing Machine DFA works.
These states also hold information about the productions they correspond to, but only by index in the @DFAProduction@ list.
Various other @HashMap@s are also stored within the DFA to keep the structure fully self referencial and standalone.
-}
module ParserGenerator.DFA where

import ParserGenerator.Grammar
import Data.HashMap.Strict
import Control.Lens

-- | The deterministic finite automata
data DFA = DFA{ _dfaStates :: [DFAState] -- ^ List of states, identified by their position in the list
              , _dfaProductions :: [DFAProduction] -- ^ All productions simplified from "Grammar"
              , _dfaTokenMap :: TokenMap -- ^ Map from token name to token pattern
              , _dfaPrecMap :: PrecMap -- ^ Map from terminal to @Prec@
              , _dfaFollowMap :: FollowMap -- ^ Map from non terminal to list of terminals
              } deriving Show

-- | Combination of @Associativity@ and a precedence level index
data Prec = Prec Associativity Int deriving Show

-- | Map from terminal to @Prec@
type PrecMap = HashMap TokenDef Prec

-- | Map from terminal name to @TokenDef@
type TokenMap = HashMap String TokenDef

-- | Map from non terminal to list of terminals
type FollowMap = HashMap String [TokenDef]

-- | Simplified Production from @RuleProduction@, now storing its own non terminal name and rid of @RuleTokenModifier@
data DFAProduction = DFAProduction{ _dfaProductionName :: String
                                  , _dfaProductionTokens :: [RuleTokenType]
                                  , _dfaProductionResult :: String
                                  , _dfaProductionPrec :: Maybe Prec
                                  , _dfaProductionResultType :: Maybe String
                                  } deriving Show

-- | Action to be taken on reading a token in the DFA
data DFAAction = DFAShift Int -- ^ Shift current token and change to state by index
               | DFAReduce Int -- ^ Reduce by production from index
               | DFAFinish -- ^ Finish reading tokens, language accepted.
               deriving (Show, Eq)

-- | DFA state includes production positions and list of actions.
data DFAState = DFAState { productions :: [(Int, Int)] -- ^ List of pairs of production index and position within production
                         , actions :: [(RuleTokenType, DFAAction)] -- ^ List of pairs of token to read followed by action to take
                         } deriving Show

-- | @DFAState@ actions are derived purely from productions, thus equality only need check productions
instance Eq DFAState where
    (DFAState ps _) == (DFAState ps' _) = ps == ps'

makeLenses ''DFA
makeLenses ''DFAProduction
