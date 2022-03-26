module Main where

-- Nome: Henrique Fantato          RA: 21053916
-- Paradigmas da Programação   -   UFABC

--          Projeto SudokuSolver - Funcao IO Main

--{-# LANGUAGE BlockArguments #-}
--{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE TemplateHaskellQuotes #-}


import SudokuSolversLib
import System.Directory
import Data.Time
import Data.List
import Control.Parallel.Strategies
import Control.Exception
import Data.Foldable (concat)
import GHC.Read (readField)



main :: IO()
main = do{ putStrLn "Bem vindo ao Sudoku Solver!";
        putStr "Este é um solver baseado em aplicacao de regras, que encontra a solucao do sudoku reduzindo ao maximo os dominios das casas do tabuleiro";
        putStrLn " e realizando a menor quantidade possivel de chutes.\n";
        putStrLn "Digite o caminho do arquivo que contém os sudokus a serem resolvidos, com o nome e o sulfixo .txt: ";
        caminho <- getLine;
        file <- tentaLerArquivo caminho;
        putStrLn "Imprimindo arquivo original:";
        putStrLn file;
        putStrLn "\n Voce deseja resolver no modo rapido, ou deseja ver o antes e depois(bonito)? Digite rapido ou bonito: ";
        modo <- getLine;
        solverModo <- escolherModo modo;
        putStrLn "Imprimindo as respostas para os Sudokus:";
        timeInit <- getCurrentTime;
        putStrLn (if solverModo == "rapido" then solverParalelo2 file else solverParalelo file);
        timeFinish <- getCurrentTime;
        putStr "Tempo necessario para solucionar os sudokus: ";
        putStrLn (show(diffUTCTime timeFinish timeInit) ++ "\n\n");

        putStrLn "Deseja resolver outro arquivo? Se sim, digite sim, caso contrario, digite sair: ";
        line <- getLine;
        if line == "sim" then main else putStrLn line}


-- Funcoes IO auxiliares da main

-- Funcao que verifica a existencia e o formato correto do arquivo recebido pela main
tentaLerArquivo :: String -> IO String
tentaLerArquivo a
    | isInfixOf ".txt" a = do check <- doesFileExist a
                              if check then readFile a else do putStrLn "Arquivo informado nao Existe"
                                                               putStrLn "Digite o caminho do arquivo com os Sudokus iniciais, com o nome do arquivo e o sulfixo .txt junto!"
                                                               b <- getLine
                                                               tentaLerArquivo b
                              
    | otherwise = do putStrLn "Caminho inválido"
                     putStrLn "Digite o caminho do arquivo com os Sudokus iniciais, com o nome do arquivo e o sulfixo .txt junto!"
                     b <- getLine
                     tentaLerArquivo b
            

-- Funcao que define o modo solver a ser executado
escolherModo :: String -> IO [Char]
escolherModo a
    | a == "rapido" = do putStrLn "Modo rapido escolhido!"
                         return "rapido"
    | a == "bonito"  = do putStrLn "Modo bonito escolhido!"
                          return "bonito"
    | otherwise     = do putStrLn "Entrada incorreta! Voce deseja resolver no modo rapido, ou deseva ver o antes e depois(lerdo)? Digite rapido ou lerdo: "
                         b <- getLine
                         escolherModo b



-- Funcoes auxiliares para formatacao do arquivo txt lido
quebraLinhas :: [Char] -> [Sudoku]
quebraLinhas file = resultados
    where
        sudokus = map divideLinhasNo9 (lines file)
        resultados = sudokus


divideLinhasNo9 :: [a] -> [[a]]
divideLinhasNo9 [] = []
divideLinhasNo9 a = take 9 a: divideLinhasNo9(drop 9 a)




-- Solvers mais complexos, que utilizam estrategia de paralelismo! 

-- Exibe a quantidade de iteracoes necessarias para resolver cada sudoku, bem como seu antes e depois
solverParalelo :: [Char] -> String
solverParalelo a = unlines(concat(parMap rpar solveUm z))
    where
        solveUm x = do (((formataTupla(head(filter ehTuplaGabarito (solver4 x))))))
            where
                ehTuplaGabarito (a, b) = a /= []
                formataTupla (a, b) = ["Sudoku inicial:\n"] ++ x ++ ["\nSolucao para o sudoku:"] ++ a ++ ["\nIteracoes necessarias para resolver o sudoku: " ++ show b ++ "\n"]

        z = quebraLinhas a

-- Exibe apenas a solucao de cada sudoku
solverParalelo2 :: [Char] -> String
solverParalelo2 a = (unlines(concat(parMap rpar solveUm z)))
    where
        solveUm b = do head(solver2 b) ++ ["\n"]
        z = quebraLinhas a