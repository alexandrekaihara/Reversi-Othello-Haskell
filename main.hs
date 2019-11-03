module Main where

import Generic
import Reversi
import Data.Char
import Interaction
import Data.Map (Map, (!))
import qualified Data.Map as Map


mapsize = 8
player  = 0


-- Compilacao: ghc --make main.hs
-- :set +m   Serve para habilitar multiplas linhas no interpretador
main :: IO()
main = do 
    showEnterDialog
    b <- initBoard mapsize
    showBoard b mapsize
    print(numberoftokens (getBoard b) (0,0) mapsize 0 0)
    print(checkEnd (getBoard b) mapsize)
    
