import Lib
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}



main :: IO ()
main = do
    
    let exampleGrammar = Grammar
            [ Rule (State "S") (Right (Terminal 'a', State "A"))
            , Rule (State "S") (Right (Terminal 'a', State "B"))
            , Rule (State "S") (Left (Terminal 'b'))
            , Rule (State "A") (Left (Terminal 'b'))
            , Rule (State "A") (Left (Terminal 'a'))
            , Rule (State "B") (Right (Terminal 'a', State "A"))
            , Rule (State "B") (Right (Terminal 'b', State "B"))
            , Rule (State "B") (Left (Terminal 'b'))
            ]
        transitions = getTransitions exampleGrammar (State "S") (Terminal 'a')
        dfa = buildDFA exampleGrammar
    putStrLn "Testing transitions!"
    print transitions
    putStrLn "Testing Automaton build!"
    printDFA dfa
    putStrLn "Well done!"