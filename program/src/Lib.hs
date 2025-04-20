{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where
import Data.List (nub)
import Data.Char (isAlpha, isLower, isSpace)
import Control.Monad (unless)
import System.IO (hIsEOF, stdin)
import Data.List.Split (splitOn)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- ���� ������ ��� ������������� ����������
newtype State = State String deriving (Eq, Ord, Show)
newtype Terminal = Terminal Char deriving (Eq, Ord, Show)
data Rule = Rule {
    lhs :: State,
    rhs :: Either Terminal (Terminal, State)
} deriving (Eq, Show)
newtype Grammar = Grammar [Rule] deriving (Show)

-- ������� �������: ������ ���� � ������ ����������
readAndParseGrammar :: IO (Either String Grammar)
readAndParseGrammar = do
    input <- readLinesUntilEOF
    return (parseGrammar input)

-- ������ ����� �� EOF
readLinesUntilEOF :: IO String
readLinesUntilEOF = do
    eof <- hIsEOF stdin
    case eof of
        True -> return ""
        False -> do
            line <- getLine
            rest <- readLinesUntilEOF
            return (line ++ "\n" ++ rest)

-- ������� ���������� �� ������
parseGrammar :: String -> Either String Grammar
parseGrammar input = do
    let cleanedInput = filter (not . isSpace) input  -- ������� ��� �������
    case cleanedInput of
        'S':xs -> do
            let ruleStrings = splitOn ";" cleanedInput
            rules <- mapM parseRule ruleStrings
            return (Grammar (nub (concat rules)))
        _ -> Left $ "First state must be S: " ++ input

-- ��������������� ������� ��� ��������
parseRule :: String -> Either String [Rule]
parseRule str = case span isAlpha str of
    ([lhsStr], '=':rest) -> do
        let lhs = State [lhsStr]
        rhsParts <- mapM parseRHS (splitOn "|" rest)
        return [Rule lhs rhs | rhs <- rhsParts]
    _ -> Left $ "Invalid rule format: " ++ str

parseRHS :: String -> Either String (Either Terminal (Terminal, State))
parseRHS [c] | isLower c = Right $ Right (Terminal c, State "F")
parseRHS (c:[s]) | isLower c && isAlpha s = Right $ Right (Terminal c, State [s])
parseRHS s = Left $ "Invalid RHS format: " ++ s
-- ���� ������ ��� ��������

newtype DFAState = DFAState [State] deriving (Eq, Ord, Show)
newtype Transition = Transition ((DFAState, Terminal), DFAState) deriving (Show)

data DFA = DFA {
    dfaStates :: [DFAState],
    dfaAlphabet :: [Terminal],
    dfaTransitions :: [Transition],
    dfaStartState :: DFAState,
    dfaAcceptStates :: [DFAState]
} deriving (Show)



-- ��������������� �������
getTerminal :: Terminal -> Char
getTerminal (Terminal c) = c

getState :: State -> String
getState (State s) = s

getDFAState :: DFAState -> [State]
getDFAState (DFAState ss) = ss

-- ������� ��� ��������� ���� ��������� �� ��������� �� �������
getTransitions :: Grammar -> State -> Terminal -> [State]
getTransitions (Grammar grammar) (State state) symbol = nub $
    [ State (getState nt') 
    | Rule nt (Right (t, nt')) <- grammar, 
      state == getState nt, t == symbol]



-- ������� ��� ���������� ���
buildDFA :: Grammar -> DFA
buildDFA (Grammar grammar) = DFA {
    dfaStates = states,
    dfaAlphabet = terminals,
    dfaTransitions = transitions,
    dfaStartState = startState',
    dfaAcceptStates = acceptStates'
}
  where
    -- �������� ��� �����������
    nonTerminals = nub $ map lhs grammar
    
    -- �������� ��� ���������
    terminals = nub ([t | Rule _ (Left t) <- grammar] ++ 
                [t | Rule _ (Right (t, _)) <- grammar])
    
    -- ��������� ��������� - ��������� ����������
    startState' = DFAState [State "S"]
    
    
    -- ���������� ���� ��������� ���
    states = [s | s <- (nub $ buildAllStates [startState'] []), s /= (DFAState [])]
      where
        buildAllStates :: [DFAState] -> [DFAState] -> [DFAState]
        buildAllStates [] visited = visited
        buildAllStates (current:rest) visited
            | current `elem` visited = buildAllStates rest visited
            | otherwise = 
                let newStates = [nub $ concat(map (\(State s) -> 
                      getTransitions (Grammar grammar) (State s) t) (getDFAState current)) | t <- terminals ]
                    allNew = nub $ newStates
                    next = rest ++ filter (\x -> not (elem x visited)) [DFAState s | s <- allNew]
                in buildAllStates next (current:visited)

    transitions = 
        [ Transition ((state, t), DFAState (nub nextStates))
        | state@(DFAState states) <- states,
          t <- terminals,
          let nextStates = concatMap (\(State s) -> getTransitions (Grammar grammar) (State s) t) states,
          not (null nextStates)]

    -- ��������� ���������
    acceptStates' = [(DFAState [State "F"])] ++ filter isAccepting states
      where
        isAccepting (DFAState states) =
            any (\(State s) -> any (\(Rule nt rhs) -> State s == nt && 
                         case rhs of 
                             Left _ -> True
                             Right (_, State "F") -> True
                             Right _ -> False) grammar) states
            || (startState' == DFAState states && 
                any (\(Rule nt rhs) -> nt == State "S") grammar)

-- ������� ��� ������ ��� � �������� ����
printDFA :: DFA -> IO ()
printDFA dfa = do
    putStrLn "DFA states:"
    print (dfaStates dfa)
    putStrLn "\nAlphabet:"
    print (dfaAlphabet dfa)
    putStrLn "\nTransitions:"
    mapM_ (\(Transition ((s, t), ns)) -> 
        putStrLn $ show s ++ " --" ++ show t ++ "--> " ++ show ns) (dfaTransitions dfa)
    putStrLn "\nStart state:"
    print (dfaStartState dfa)
    putStrLn "\nAccept state:"
    print (dfaAcceptStates dfa)