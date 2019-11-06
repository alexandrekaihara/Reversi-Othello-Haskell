module Interaction where

import Generic
import Data.Map (Map, (!))
import qualified Data.Map as Map


{- --------------------------------------------
    Funções de interação com terminal
    ---------------------------------------------}

{-Comando para dar scape no terminal-}
cleanScreen :: IO()
cleanScreen = putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

{-Imprime as strings de uma lista de string-}
showLines :: [String] -> IO()
showLines [] = return ()
showLines s = do 
    putStrLn  (head s)
    showLines (tail s)

{-Exibe mensagem inicial do jogo-}
showEnterDialog :: IO ()
showEnterDialog = do
    cont <- readFile ".entermsg"
    showLines (take 25 (lines cont))

{-Mensagem de vitória-}
showDefeat :: IO()
showDefeat = do
    cont <- readFile ".entermsg"
    showLines (take 7 (drop 25 (lines cont)))

{-Mensagem de derrota-}
showVictory :: IO()
showVictory = do
    cont <- readFile ".entermsg"
    showLines (take 7 (drop 32 (lines cont)))

{-Mensagem de derrota-}
showOutOfMoves :: IO()
showOutOfMoves = do
    cont <- readFile ".entermsg"
    showLines (take 3 (drop 39 (lines cont)))

{-Retorna a string com a visão do usuário de seu tabuleiro-}
showB :: Map (Int,Int) Char -> (Int,Int) -> Int -> String
showB m t@(i,j) mapsize 
    {-Imprimindo as bordas esquerdas-}
    | i == 0    = if (j < mapsize)
                    then if (Map.notMember t m)
                        then (" " ++ show2Dig (j)) ++ "\t│ |"  ++ (showB m (i+1,j) mapsize)
                        else (" " ++ show2Dig (j)) ++ "\t│"  ++ (m ! (i,j):"|") ++ (showB m (i+1,j) mapsize)
                    else ""
    {-Imprimindo as bordas direitas-}
    | i == mapsize = if (j < mapsize)
                    then ("│\n" ++ showB m (0,j+1) mapsize)
                    else "│\n"
    {-Imprimindo conteudo das casas-}
    | otherwise = if(i == (mapsize - 1))
                    then if (Map.notMember t m)
                        then ((' ') : ("" ++ showB m (i+1,j) mapsize))
                        else ((m ! (i,j):"") ++ showB m (i+1,j) mapsize)
                    else if (Map.notMember t m)
                        then ((' ') : ("|" ++ showB m (i+1,j) mapsize))
                        else ((m ! (i,j):"|") ++ showB m (i+1,j) mapsize)


{- --------------------------------------------
    Funções de interação com usuário
    ---------------------------------------------}

{-Lê uma opção (Char) do teclado que seja menor que limit-}
getOpt :: Int -> IO String
getOpt limit = do
    x <- getLine
    let option = (read x)
    if (option < limit)
        then return x
        else do
            putStrLn " Opção inválida. Por favor selecionar novamente.\n"
            getOpt limit




            