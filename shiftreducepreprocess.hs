{-|
Module      : ShiftReducePreProcess
Description : Pre-processing of the Grammar for Shift-Reduce.
Copyright   : (c) Samuel Williams, 2021
License     : GPL-3
Maintainer  : samuel.will1999@gmail.com
Stability   : release

Handles changing modifiers to simple rules, building the various @HashMap@s needed, and ensuring the Grammar is semantically well formed.
-}
module ShiftReducePreProcess ( handleModifiers
                             , makeTokenMap
                             , makePrecMap
                             , makeProductions
                             , makeFollowMap
                             , getNonTerminals ) where

import DFA
import Grammar
import ParserRequirements
import Data.HashMap.Strict as Map hiding (foldr, filter)
import Data.Hashable
import Data.Maybe
import Data.List hiding (insert)
import Data.List.Split
import Control.Lens

-- handleRuleModifiers on each rule, concat results
-- | Converts all token modifiers to extra rules, giving a modifier-less grammar.
handleModifiers :: Grammar -> Grammar
handleModifiers gmr = gmr{ rules=nub $ concat newRules }
  where
    newRules = Prelude.map handleRuleModifiers $ rules gmr

-- Call handleProductionModifiers on each production of a rule
handleRuleModifiers :: Rule -> [Rule]
handleRuleModifiers rule = (rule{ ruleProductions=concat prods }):(concat newRules)
  where
    prodRulePairs = Prelude.map handleProductionModifiers $ ruleProductions rule
    (prods, newRules) = unzip prodRulePairs

-- Rule builders for token modifiers

makeSomeRule :: RuleToken -> Maybe RuleTokenType -> Rule
makeSomeRule token@(RuleToken tType _) sep = Rule{ -- Add Sep to tokens of first production and make v2 -> v3 if sep isnt Nothing
    ruleName=someRuleName,
    ruleProductions=[
        RuleProduction{
            productionTokens=
                [normalToken] ++
                fmap ruleToken (maybeToList sep) ++
                [nonTerminalToken someRuleName]
            ,
            productionResult=if sep == Nothing then "v1:v2" else "v1:v3",
            productionPrecToken=Nothing
        },
        RuleProduction{
            productionTokens=[normalToken],
            productionResult="[v1]",
            productionPrecToken=Nothing
        }
    ]}
  where
    someRuleName = concat ["+", getSepStr sep, tokenName]
    tokenName = getTokenStr token
    normalToken = RuleToken tType RuleTokenModifierNormal

makeJustRule :: RuleToken -> Rule
makeJustRule token@(RuleToken tType _) = Rule{
    ruleName='?':tokenName,
    ruleProductions=[
        RuleProduction{
            productionTokens=[
                normalToken
            ],
            productionResult="Just v1",
            productionPrecToken=Nothing
        }
    ]}
  where
    tokenName = getTokenStr token
    normalToken = RuleToken tType RuleTokenModifierNormal

makeEmptyRule :: RuleToken -> Rule
makeEmptyRule token = Rule{
    ruleName='-':tokenName,
    ruleProductions=[
        RuleProduction{
            productionTokens=[],
            productionResult="empty",
            productionPrecToken=Nothing
        }
    ]}
  where
    tokenName = getTokenStr token

getSepStr :: Maybe RuleTokenType -> String
getSepStr Nothing = ""
getSepStr (Just x) = "(" ++ getTokenTypeStr x ++ ")"

-- Recurse over tokens of a production, modifying said rules productions and the global rule list as needed
handleProductionModifiers :: RuleProduction -> ([RuleProduction], [Rule])
handleProductionModifiers prod@(RuleProduction [] _ _) = ([prod], [])
handleProductionModifiers prod@(RuleProduction (x:xs) _ _) =
    case tokenModifier x of
        RuleTokenModifierNormal -> ( conToken x newProds, newRules )
        RuleTokenModifierSome sep -> ( prodsPrefixNonTerminal ("+" ++ getSepStr sep) newProds, (makeSomeRule x sep):newRules )
        RuleTokenModifierMany sep -> ( prodsPrefixNonTerminal ("+" ++ getSepStr sep) newProds ++ prodsPrefixNonTerminal "-" newProds, (makeEmptyRule x):(makeSomeRule x sep):newRules )
        RuleTokenModifierOptional -> ( prodsPrefixNonTerminal "?" newProds ++ prodsPrefixNonTerminal "-" newProds, (makeEmptyRule x):(makeJustRule x):newRules )
  where
    ( newProds, newRules ) = handleProductionModifiers $ prod{ productionTokens=xs }
    conToken x prods = Prelude.map (\prod' -> prod'{ productionTokens=x:(productionTokens prod') }) prods
    prodsPrefixNonTerminal c prods = conToken (nonTerminalToken $ c ++ (getTokenStr x)) prods

-- Pre processing

-- | Builds a @HashMap@ from a list of @TokenDef@s, erroring on multiple definitions and invalid patterns
makeTokenMap :: [TokenDef] -> Result TokenMap
makeTokenMap [] = return $ singleton "%EOF" $ TokenDef "%EOF" "TokenEOF"
makeTokenMap (t:ts) = do
    rest <- makeTokenMap ts

    if member (tokenName t) rest then
        Error $ "Multiple defintions of token " ++ tokenName t
    else if (length $ splitOn "$$" $ tokenPattern t) > 2 then
        Error $ "More than one $$ definition in token " ++ tokenName t
    else
        return $ insert (tokenName t) t rest

indexHashMap :: (Eq a, Hashable a) => HashMap a b -> a -> String -> Result b
indexHashMap hm k err = case hm^.at k of
                            Just v  -> Result v
                            Nothing -> Error err

-- | Builds a @HashMap@ from TokenDef to Prec using the precedence levels
-- Errors if a token is present in multiple precedence levels
makePrecMap :: [PrecLevel] -> TokenMap -> Result PrecMap
makePrecMap [] _ = return mempty
makePrecMap (p:ps) tm = do
    rest <- makePrecMap ps tm

    let prec = Prec (levelAssociativity p) (length ps + 1)

    updatedRest <- addPrecToMap (levelTokens p) prec tm rest

    return updatedRest

addPrecToMap :: [String] -> Prec -> TokenMap -> PrecMap -> Result PrecMap
addPrecToMap [] _ _ pm = return pm
addPrecToMap (x:xs) p tm pm = do
    rest <- addPrecToMap xs p tm pm
    token <- indexHashMap tm x $ "Precedence defined for token " ++ x ++ " which does not exist"

    if member token pm then
        Error $ "Multiple precedence definitions for token " ++ x
    else
        return $ insert token p rest

-- production list generation
ruleNonTerminalCheck :: String -> [Rule] -> Result RuleTokenType
ruleNonTerminalCheck r rs = do
    case length $ Prelude.filter (\t -> ruleName t == r) rs of
        0         -> Error $ "No such rule " ++ r
        1         -> Result $ RuleNonTerminal r
        otherwise -> Error $ "Multiple definitions of rule " ++ r

-- | Converts rules into a list of @DFAProduction@s, checking all tokens within the rule are valid, and assigning the correct precedences.
makeProductions :: [Rule] -> [Rule] -> TokenMap -> PrecMap -> Result [DFAProduction]
makeProductions [] _ _ _ = return []
makeProductions (r:rs) rs' tm pm = do
    rest <- makeProductions rs rs' tm pm

    productions <- fmap concat $ mapM productionsFromRule $ ruleProductions r

    return $ productions ++ rest
  where
    productionsFromRule p = do
        tokens <- getTokens $ productionTokens p

        precM <- case productionPrecToken p of
            Nothing -> findProdPrec tokens tm pm
            Just t -> fmap Just $ tokenToPrec t tm pm

        return [DFAProduction (ruleName r) tokens (productionResult p) precM]

    getTokens ts = do
        mapM (\t -> case tokenType t of
                        t'@(RuleTerminal s) -> indexHashMap tm s ("No such terminal: " ++ s) >> return t'
                        t'@(RuleNonTerminal r) -> ruleNonTerminalCheck r rs') ts

findProdPrec :: [RuleTokenType] -> TokenMap -> PrecMap -> Result (Maybe Prec)
findProdPrec ts tm pm = return $ (find isTerminal $ reverse ts) >>= (\(RuleTerminal t') -> pm^.at (tm ! t'))

tokenToPrec :: String -> TokenMap -> PrecMap -> Result Prec
tokenToPrec t tm pm = do
    token <- indexHashMap tm t $ "No such token " ++ t ++ " used in %prec"
    indexHashMap pm token $ "Token " ++ t ++ " has no precedence defined"

isTerminal :: RuleTokenType -> Bool
isTerminal (RuleTerminal _) = True
isTerminal _ = False

-- | Gets the names of all Non-Terminals from a list of @DFAProduction@s
getNonTerminals :: [DFAProduction] -> [String]
getNonTerminals = nub . (fmap $ view dfaProductionName)

concatNub :: Eq a => [a] -> [a] -> [a]
concatNub a b = nub $ a ++ b

addFollowers :: [DFAProduction] -> TokenMap -> String -> FollowMap -> FollowMap
addFollowers ps tm nt fm = if member nt fm then fm else
    aux (Map.insert nt [] fm) ps
  where
    aux fm [] = fm
    aux fm (p:ps') = aux (aux' fm p $ p ^. dfaProductionTokens) ps'
    aux' fm p [] = fm
    aux' fm p (t:ts)
        | t == RuleNonTerminal nt = case addNextTerminals ts ps tm of
            (followers, ts', True) -> if prodName == nt then aux' fm' p ts' else aux' (Map.insertWith concatNub nt (fm'' ! prodName) fm'') p ts'
              where
                fm' = Map.insertWith concatNub nt followers fm
                fm'' = addFollowers ps tm prodName fm'
                prodName = p ^. dfaProductionName
            (followers, ts', False) -> aux' (Map.insertWith concatNub nt followers fm) p ts'
        | otherwise = aux' fm p ts

addNextTerminals :: [RuleTokenType] -> [DFAProduction] -> TokenMap -> ([TokenDef], [RuleTokenType], Bool)
addNextTerminals [] ps tm = ([], [], True)
addNextTerminals ts@((RuleTerminal t):_) ps tm = ([tm ! t], ts, False)
addNextTerminals ((RuleNonTerminal t):ts) ps tm = if dangling then (nub $ tds ++ tds', ts', True) else (tds, (RuleNonTerminal t):ts, False)
  where
    (dangling, tds) = nextTerminals ps tm t
    (tds', ts', _) = addNextTerminals ts ps tm

nextTerminals :: [DFAProduction] -> TokenMap -> String -> (Bool, [TokenDef])
nextTerminals ps tm nt = auxNext [] nt
  where
    aux checked []     = (True, [])
    aux checked (t:ts) = case t of
        RuleTerminal t'    -> (False, [tm ! t'])
        RuleNonTerminal t' -> if elem t' checked then (False, []) else
            if dangling then combine [(False, terminals), aux checked ts] else (False, terminals)
          where
            (dangling, terminals) = auxNext checked t'
    combine = foldr (\(b, xs) (b', xs') -> (b || b', nub $ xs ++ xs')) (False, [])
    getProductions nt' = filter (view $ dfaProductionName.(to (==nt'))) ps
    auxNext checked nt' = combine $ fmap ((aux (nt':checked)) . view dfaProductionTokens) $ getProductions nt'

-- | Calculates all the follower tokens for every Non-Terminal in a list of productions.
-- Followers are defined as terminals that can be read directly after a Non-Terminal, taking all @DFAProduction@s into account.
-- More information about how to calculate this can be found here: <http://www.cs.nuim.ie/~jpower/Courses/Previous/parsing/node48.html>
makeFollowMap :: [DFAProduction] -> TokenMap -> FollowMap
makeFollowMap ps tm = aux $ getNonTerminals ps
  where
    aux [] = empty
    aux (t:ts) = addFollowers ps tm t $ aux ts