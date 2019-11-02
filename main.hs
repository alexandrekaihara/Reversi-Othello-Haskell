module Main where

    {-Data import-}
    import Data.Char
    import Data.Map (Map, (!))
    import qualified Data.Map as Map
    {-This project imports-}
    import Generic
    import Reversi

-- Compiacao: ghc --make main.hs
-- :set +m   Serve para habilitar multiplas linhas no interpretador

sizemap = 8

main = do 
{
    let a = line (create 4 4 (0, ' ')) 3 
    in  print (a)
    
}
