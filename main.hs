module Main where

import Generic
import Reversi
import Data.Char
import Interaction
import Data.Map (Map, (!))
import qualified Data.Map as Map


-- Compilacao: ghc main
-- :set +m   Serve para habilitar multiplas linhas no interpretador
main :: IO()
main = do 
    {-Exibe mensagem de início-}
    showEnterDialog
    getChar
    cleanScreen
    
    let twoplayer = 1   {-Adicionar opção de escolher 1 ou 2 players -}
    let mapsize = 8     {-Adicionar opção de escolher tamanho do mapa-}
    rv <- initBoard mapsize
    playRV rv mapsize 0 twoplayer 0

