module Main (main) where

import Lib_types()
import Lib_functions
import System.IO

{-
  Grammar input specification:

  1. The input is a single string representing the grammar.

  2. The grammar consists of several rules, each in the form:
     <NonTerminal> = <Alternative> | <Alternative> | ...

  3. Each rule ends with a semicolon (';') with exception of the last rule (it ends with space symbol).

  4. Whitespace may be completely absent in the input or there are may be several:
       S=aN|bN;N=cN|d
     as well as:
       S = a N | b N ; N = c N | d

  5. On the left side of each rule is a nonterminal — a single uppercase letter.

  6. The **first rule** in the grammar **must be for the nonterminal `S`**.

  7. Each alternative on the right-hand side consists of **one terminal**
     (a lowercase letter), optionally followed by **one nonterminal**
     (an uppercase letter).

     Valid examples:
       - `a`    — terminal only
       - `aN`   — terminal followed by a nonterminal

     Invalid examples:
       - `Na`   — nonterminal before terminal
       - `abN`  — several terminals
       - `aNN`  — several nonterminals

   8. To finish input use EOF:
       - Ctrl + Z -> Enter - for windows
       - Ctrl + D - other
-}

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
    hFlush stdout