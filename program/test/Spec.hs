{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Lib
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    
    let exampleGrammar = Grammar
            [ Rule (State "S") (Right (Terminal 'a', State "A"))
            , Rule (State "S") (Right (Terminal 'a', State "B"))
            , Rule (State "S") (Right (Terminal 'b', State "F"))
            , Rule (State "A") (Right (Terminal 'b', State "F"))
            , Rule (State "A") (Right (Terminal 'a', State "F"))
            , Rule (State "B") (Right (Terminal 'a', State "A"))
            , Rule (State "B") (Right (Terminal 'b', State "B"))
            , Rule (State "B") (Right (Terminal 'b', State "F"))
            ]
        transitions = getTransitions exampleGrammar (State "S") (Terminal 'a')
        dfa = buildDFA exampleGrammar
    putStrLn "Testing transitions!"
    print transitions
    putStrLn "Testing Automaton build!"
    printDFA dfa
    putStrLn "Well done!"
    putStrLn "----------------------------\n\n"
    putStrLn "Lets try parsing!"
    parsed <- readAndParseGrammar
    case parsed of
        Left err -> error err
        Right gram -> putStrLn (show gram)
    putStrLn "\n\nWow! Not bad!"
    putStrLn "----------------------------\n\n"
    putStrLn "Should we try building DFA with dat one???? :))))"
    putStrLn "Ill do it anyway"

    case parsed of
        Right gram -> printDFA (buildDFA gram)

    hFlush stdout