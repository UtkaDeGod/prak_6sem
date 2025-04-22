module Main (main) where

import Lib
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "Write some correct rules!"
    parsed <- readAndParseGrammar
    case parsed of
        Left err -> error err
        Right gram -> do
            putStrLn (show gram)
            putStrLn "\n\nWow! Not bad!"
            putStrLn "----------------------------\n\n"
            putStrLn "Should we try building DFA with dat one???? :))))"
            putStrLn "Ill do it anyway"
            printDFA (buildDFA gram)
            putStrLn "\n\nNice job! ;)))"
    hFlush stdout