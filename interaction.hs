module Interaction where

import Generic
import Data.Map (Map, (!))
import qualified Data.Map as Map


{- --------------------------------------------
    Funções de interação com terminal
    ---------------------------------------------}

{-Comando para dar scape no terminal-}
cleanScreen :: IO()
cleanScreen = putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

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

{-Retorna a string com a visão do usuário de seu tabuleiro-}
showB :: Map (Int,Int) Char -> (Int,Int) -> Int -> String
showB m t@(i,j) mapsize 
    {-Imprimindo as bordas esquerdas-}
    | j == 0    = if (i < mapsize)
                    then if (Map.notMember t m)
                        then (" " ++ show2Dig (i)) ++ "\t│ |"  ++ (showB m (i,j+1) mapsize)
                        else (" " ++ show2Dig (i)) ++ "\t│"  ++ (m ! (i,j):"|") ++ (showB m (i,j+1) mapsize)
                    else ""
    {-Imprimindo as bordas direitas-}
    | j == mapsize = if (i < mapsize)
                    then ("│\n" ++ showB m (i+1,0) mapsize)
                    else "│\n"
    {-Imprimindo conteudo das casas-}
    | otherwise = if(j == (mapsize - 1))
                    then if (Map.notMember t m)
                        then ((' ') : ("" ++ showB m (i,j+1) mapsize))
                        else ((m ! (i,j):"") ++ showB m (i,j+1) mapsize)
                    else if (Map.notMember t m)
                        then ((' ') : ("|" ++ showB m (i,j+1) mapsize))
                        else ((m ! (i,j):"|") ++ showB m (i,j+1) mapsize)


{- --------------------------------------------
    Funções de interação com usuário
    ---------------------------------------------}

{-Lê uma opção (Char) do teclado e valida-}
getOpt :: Int -> IO String
getOpt mapsize= do
    x <- getLine
    let option = (read x)
    if (option < mapsize)
        then return x
        else do
            putStrLn " Opção inválida. Por favor selecionar novamente."
            getOpt mapsize


