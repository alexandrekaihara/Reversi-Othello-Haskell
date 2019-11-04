module Main where

import Generic
import Reversi
import Data.Char
import Interaction
import Data.Map (Map, (!))
import qualified Data.Map as Map


mapsize = 30


-- Compilacao: ghc main
-- :set +m   Serve para habilitar multiplas linhas no interpretador
main :: IO()
main = do 
    {-Exibe mensagem de início-}
    showEnterDialog
    getChar
    cleanScreen
    
    let twoplayer = 1 {-Adicionar opção de escolher 1 ou 2 players-}
    
    rv <- initBoard mapsize
    playRV rv mapsize 0 twoplayer

