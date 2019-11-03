module Main where

import Generic
import Reversi
import Data.Char
import Interaction
import Data.Map (Map, (!))
import qualified Data.Map as Map


mapsize = 8
player  = 0


-- Compilacao: ghc main
-- :set +m   Serve para habilitar multiplas linhas no interpretador
main :: IO()
main = do 
    {-Exibe mensagem de início-}
    showEnterDialog
    getChar

    {-Verificando se o jogador quer posicionar os navios-}
    cleanScreen
    b <- initBoard mapsize
    showBoard b mapsize
    print(numberoftokens (getBoard b) (0,0) mapsize 0 0)
    print(checkEnd (getBoard b) mapsize)
    a <- (getMove b mapsize 0)
    print (a)
    
