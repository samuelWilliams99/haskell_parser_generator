module ShiftReduce (module ShiftReducePreProcess, generateDFA) where

import DFA
import Grammar
import ParserRequirements
import ShiftReducePreProcess
import Data.HashMap.Strict as Map hiding (foldr, filter)
import Data.Maybe
import Data.List hiding (insert)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Safe hiding (at)
import Control.Lens

is x = to (==x)
elementF i = to (!!i)

deleteAt :: Int -> [a] -> [a]
deleteAt 0 (_:xs) = xs
deleteAt n (x:xs) = x:deleteAt (n - 1) xs
deleteAt _ []     = []

-- State machine generation

generateDFA :: Grammar -> Result DFA
generateDFA g = do
    let (Grammar ts ps rs) = handleModifiers g
    tokenMap <- makeTokenMap ts
    precMap <- makePrecMap ps tokenMap
    prods <- makeProductions rs rs tokenMap precMap

    let prods' = (DFAProduction "START" [RuleNonTerminal $ prods^._head.dfaProductionName, RuleTerminal "%EOF"] "" Nothing):prods

    let followMap = makeFollowMap prods' tokenMap

    makeStateMachine $ DFA [] prods' tokenMap precMap followMap

makeStateMachine :: DFA -> Result DFA
makeStateMachine dfa = fmap snd $ runStateT makeStateMachineAux dfa

type StateResult a = StateT DFA Result a

-- Progress a state from reading a token
progressState :: DFAState -> RuleTokenType -> StateResult DFAState
progressState s t = do
    prods <- use dfaProductions
    return $ DFAState (aux prods $ productions s) []
  where
    onProd prods p f = prods^.(elementF p).dfaProductionTokens.to f
    aux prods [] = []
    aux prods ((p, p'):ps) | onProd prods p length == p' = aux prods ps
                           | onProd prods p (!!p') == t  = (p, p' + 1):(aux prods ps)
                           | otherwise                   = aux prods ps

-- Generate list of actions for reduction using followers
getReduceActions :: Int -> StateResult [(RuleTokenType, DFAAction)]
getReduceActions i = do
    prods <- use dfaProductions
    followMap <- use dfaFollowMap
    let followers = followMap ! (prods^.(elementF i).dfaProductionName)
    let followerRuleTokens = fmap (RuleTerminal . tokenName) followers
    return $ zip followerRuleTokens $ repeat $ DFAReduce i

isShift :: DFAAction -> Bool
isShift (DFAShift _) = True
isShift _ = False

-- Get the precedence of an action, be that action a shift or a reduce.
getPrec :: DFAAction -> RuleTokenType -> StateResult (Maybe Prec)
getPrec DFAFinish _ = return Nothing
getPrec (DFAShift _) t = do
    pm <- use dfaPrecMap
    tm <- use dfaTokenMap
    case t of
        RuleTerminal t' -> return $ pm^.at (tm ! t')
        otherwise       -> return Nothing
getPrec (DFAReduce i) _ = use $ dfaProductions.(elementF i).dfaProductionPrec

-- Compare two precs, return if left takes priority, fail if unknown
precCompare :: Prec -> Prec -> Maybe Bool
precCompare (Prec assoc p) (Prec _ p') = if p > p' then Just True
    else if p < p' then Just False
    else if assoc == LeftAssoc then Just False
    else if assoc == RightAssoc then Just True
    else Nothing

maybeError :: String -> Maybe a -> Result a
maybeError e Nothing = Error e
maybeError _ (Just a) = Result a

getReduceIndex :: DFAAction -> DFAAction -> Int
getReduceIndex (DFAReduce n) _ = n
getReduceIndex _ (DFAReduce n) = n

getShiftIndex :: DFAAction -> DFAAction -> Int
getShiftIndex (DFAShift n) _ = n
getShiftIndex _ (DFAShift n) = n

-- Combine 2 lists of actions, looking for clashes on read tokens.
-- On clash, deal with conflicts either by erroring or picking the correct one using precedence and associativity
combineActions :: [(RuleTokenType, DFAAction)] -> [(RuleTokenType, DFAAction)] -> StateResult [(RuleTokenType, DFAAction)]
combineActions xs [] = return xs
combineActions xs (y@(yt, ya):ys) = case findIndex ((==yt) . fst) xs of
    Nothing -> combineActions (xs ++ [y]) ys
    Just i -> do
        let xa = snd $ xs !! i
        if isShift xa && isShift ya then
            if xa == ya then
                combineActions xs ys
            else
                lift $ Error $ "Shift-shift error, how on earth did you do this, this shouldn't be possible."
        else if isShift xa /= isShift ya then do
            xPrec <- getPrec xa yt
            yPrec <- getPrec ya yt
            prods <- use dfaProductions
            if isJust xPrec && isJust yPrec then do
                let err = "Shift reduce error between " ++ show xa ++ " and " ++ show ya ++ " on " ++ show yt
                leftBigger <- lift $ maybeError err $ precCompare (fromJust xPrec) (fromJust yPrec)
                if leftBigger then
                    combineActions xs ys
                else
                    fmap (y:) $ combineActions (deleteAt i xs) ys
            else do
                states <- use dfaStates
                lift $ Error $ "Unrecoverable shift-reduce error between " ++ show xa ++ " and " ++ show ya ++ " on " ++ show yt ++
                               ". This can be solved using %prec on the rule or precedence definitions for the tokens.\n\n" ++
                               (show $ prods !! (getReduceIndex xa ya)) ++ "\n\n" ++
                               (show $ states !! (getShiftIndex xa ya))
        else
            lift $ Error $ "Reduce-reduce error between " ++ show xa ++ " and " ++ show ya

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n = set $ element n

-- Generate the list of actions for a state, building new states whenever one is needed and does not yet exist
-- Add reduction actions for complete productions
-- Add shift actions for incomplete productions, building new state and recursively adding it's state actions.
addStateActions :: DFAState -> StateResult DFAState
addStateActions s = do
    prods <- use dfaProductions
    as <- aux prods $ productions s
    return $ s { actions = as }
  where
    aux prods [] = return []
    aux prods ((p, p'):ps)
      | prods^.(elementF p).dfaProductionTokens.to length.is p' = do
            reduceActions <- getReduceActions p
            rest <- aux prods ps
            combineActions rest reduceActions
      | elem (p, p') ps = aux prods ps
      | otherwise = do
            let prodTokens = prods^.(elementF p).dfaProductionTokens
            let next = prodTokens !! p'
            rest <- aux prods ps
            s' <- progressState s next
            s' <- expandStateProds s'
            states <- use dfaStates
            case findIndex (==s') states of
                Just i -> combineActions rest [(next, DFAShift i)]
                Nothing -> if p == 0 && p' == length prodTokens - 1 then
                        combineActions rest [(next, DFAFinish)]
                    else do
                        let i = length states
                        -- Add the state to the stack before recursing, in case the recurse ends up making a copy
                        assign dfaStates $ states ++ [s']
                        s'' <- addStateActions s'
                        assign (dfaStates.element i) s''
                        combineActions rest [(next, DFAShift i)]

-- Build first state using first production
getInitialState :: StateResult DFAState
getInitialState = do
    prods <- use dfaProductions
    let initProds = getProductionIndicesByName (head $ getNonTerminals prods) prods
    let state = DFAState (zip initProds $ repeat 0) []
    state <- expandStateProds state
    return state

-- Get first state, expand it then return.
makeStateMachineAux :: StateResult ()
makeStateMachineAux = do
    state <- getInitialState
    assign dfaStates [state]
    state' <- addStateActions state
    assign (dfaStates.element 0) state'
    return ()

-- Get indices of productions with given name
getProductionIndicesByName :: String -> [DFAProduction] -> [Int]
getProductionIndicesByName name = findIndices $ view $ dfaProductionName.is name

-- Search productions for any that next expect a non-terminal, and add productions for those non-terminals
expandStateProds :: DFAState -> StateResult DFAState
expandStateProds (DFAState ps as) = fmap (\ps' -> DFAState ps' as) $ aux 0 ps
  where
    aux n ps'
      | n == length ps' = return ps'
      | otherwise = do
        dfa <- get
        prods <- use dfaProductions
        let (p, p') = ps' !! n
        case (prods^.(elementF p).dfaProductionTokens) `atMay` p' of
            Just (RuleNonTerminal nt) -> aux (n+1) new
              where
                new = nub $ ps' ++ (zip (getProductionIndicesByName nt prods) (repeat 0))
            otherwise -> aux (n+1) ps'
