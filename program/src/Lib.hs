{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where


someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- ���� ������ ��� ������������� ����������
newtype State = State String deriving (Eq, Ord, Show)
newtype Terminal = Terminal Char deriving (Eq, Ord, Show)
data Rule = Rule {
    lhs :: State,
    rhs :: Either Terminal (Terminal, State)
} deriving (Show)
newtype Grammar = Grammar [Rule] deriving (Show)


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


-- �������� ���������� �� ������
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub (x:xs)
  | x `elem` xs = myNub xs
  | otherwise = x : myNub xs

-- �������� �������
myDifference :: Eq a => [a] -> [a] -> [a]
myDifference xs ys = filter (`notElem` ys) xs

-- ����� � ������������� ������
myLookup :: Eq k => k -> [(k, v)] -> Maybe v
myLookup _ [] = Nothing
myLookup key ((k,v):xs)
  | key == k = Just v
  | otherwise = myLookup key xs

-- ��������������� �������
getTerminal :: Terminal -> Char
getTerminal (Terminal c) = c

getState :: State -> String
getState (State s) = s

getDFAState :: DFAState -> [State]
getDFAState (DFAState ss) = ss

-- ������� ��� ��������� ���� ��������� �� ��������� �� �������
getTransitions :: Grammar -> State -> Terminal -> [State]
getTransitions (Grammar grammar) (State state) symbol = myNub $
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
    nonTerminals = myNub $ map lhs grammar
    
    -- �������� ��� ���������
    terminals = myNub ([t | Rule _ (Left t) <- grammar] ++ 
                [t | Rule _ (Right (t, _)) <- grammar])
    
    -- ��������� ��������� - ��������� ����������
    startState' = DFAState [State "S"]
    
    
    -- ���������� ���� ��������� ���
    states = myNub $ buildAllStates [startState'] []
      where
        buildAllStates :: [DFAState] -> [DFAState] -> [DFAState]
        buildAllStates [] visited = visited
        buildAllStates (current:rest) visited
            | current `elem` visited = buildAllStates rest visited
            | otherwise = 
                let newStates = [ concat(map (\(State s) -> 
                      getTransitions (Grammar grammar) (State s) t) (getDFAState current)) | t <- terminals ]
                    allNew = myNub $ newStates
                    next = rest ++ filter (not . (`elem` visited)) [DFAState s | s <- allNew]
                in buildAllStates next (current:visited)

    transitions = 
        [ Transition ((state, t), DFAState (myNub nextStates))
        | state@(DFAState states) <- states,
          t <- terminals,
          let nextStates = concatMap (\(State s) -> getTransitions (Grammar grammar) (State s) t) states,
          not (null nextStates)]

    -- ��������� ���������
    acceptStates' = filter isAccepting states
      where
        isAccepting (DFAState states) =
            any (\(State s) -> any (\(Rule nt rhs) -> State s == nt && 
                         case rhs of 
                             Left _ -> True
                             Right _ -> False) grammar) states
            || (startState' == DFAState states && 
                any (\(Rule nt rhs) -> nt == State "S" && 
                                 case rhs of 
                                     Left _ -> False
                                     Right _ -> False) grammar)

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