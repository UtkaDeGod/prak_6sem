module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Epsilon = Epsilon Char
newtype State = State Char
newtype Terminal = Terminal Char | Epsilon

newtype Transition = Transition (State, Terminal, State)
newtype Grammar = Grammar [Transition]
newtype Automaton = Automaton [String]


parseGram :: String -> IO (Either String Grammar)

normalizeGram :: Grammar -> Grammar

buildAuto :: Grammar -> Automaton