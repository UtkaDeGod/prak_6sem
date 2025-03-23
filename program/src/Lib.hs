module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype State = State Char
newtype Terminal = Terminal Char

newtype Transition = Transition (State, Terminal, State)
newtype Grammar = Grammar [Transition]
newtype Automaton = Automaton [String]


parseGram :: String -> Grammar

normalizeGram :: Grammar -> Grammar

buildAuto :: Grammar -> Automaton