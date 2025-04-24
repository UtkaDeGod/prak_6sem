module Lib_types (
    State(..),
    Terminal(..),
    Rule(..),
    Grammar(..),
    DFAState(..),
    Transition(..),
    DFA(..)
) where


-- Data types for representing grammar
newtype State = State String deriving (Eq, Ord, Show)
newtype Terminal = Terminal Char deriving (Eq, Ord, Show)
data Rule = Rule {
    lhs :: State,
    rhs :: Either Terminal (Terminal, State)
} deriving (Eq, Show)
newtype Grammar = Grammar [Rule] deriving (Show)

-- Data types for DFA
newtype DFAState = DFAState [State] deriving (Eq, Ord, Show)
newtype Transition = Transition ((DFAState, Terminal), DFAState) deriving (Show)

data DFA = DFA {
    dfaStates :: [DFAState],
    dfaAlphabet :: [Terminal],
    dfaTransitions :: [Transition],
    dfaStartState :: DFAState,
    dfaAcceptStates :: [DFAState]
} deriving (Show)
