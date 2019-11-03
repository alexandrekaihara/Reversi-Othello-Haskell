module Reversi where

import Generic
import Data.Char
import Interaction
import Data.Map (Map, (!))
import qualified Data.Map as Map


{-Representação do jogo: Tabuleiro e duas listas de movimentos possíveis.-}
data Reversi = Reversi (Board) Moves deriving (Show)

{-Tabuleiro dos navios do usuário-}
type Board  = Map (Int,Int) Char
{-Lista de tuplas, para o pc e o player saberem as jogadas possíveis-}
type Moves = [(Int,Int)] 


{- --------------------------------------------
   Funções de manipulação do tabuleiro do jogo
   ---------------------------------------------}

{-Cria um novo tabuleiro vazio-}
newBoard :: IO Reversi
newBoard = return (Reversi Map.empty [])

{-Inicializa os tabuleiros de jogo de acordo com o tamanho do mapa-}
initBoard :: Int -> IO Reversi
initBoard mapsize = do
    let position = (quot mapsize 2)
    let coord = [(position-1, position-1), (position, position-1), (position-1, position), (position, position)]
    let caractere = [player 0, player 1, player 1, player 0] 
    return (Reversi (mapInsert coord caractere) [])

{-Exibe o tabuleiro do usuário-}
showBoard :: Reversi -> Int -> IO()
showBoard (Reversi b m) mapsize = putStr (printlines (showB b (0,0) mapsize) 0 mapsize)

{-Retorna o tabuleiro do jogo-}
getBoard :: Reversi -> Map (Int,Int) Char
getBoard (Reversi b m) = b

{-Insere uma lista de tuplas com seu respectivo char no mapa-}
mapInsert :: [(Int,Int)] -> [Char] -> Map (Int,Int) Char
mapInsert [] [] = Map.empty
mapInsert (h1:tl1) (h2:tl2) = Map.insert h1 h2 (mapInsert tl1 tl2)

{-Conta o numero peças no tabuleiro-}
numberoftokens :: Map (Int,Int) Char -> (Int, Int) -> Int -> Int -> Int -> (Int, Int)
numberoftokens m t@(i,j) mapsize numberofO numberofX = do
    if(j <  mapsize)
        then if (Map.notMember t m)
            then numberoftokens m (i,j+1) mapsize numberofO numberofX
            else if ((m ! (i,j)) == 'O')
                then numberoftokens m (i,j+1) mapsize (numberofO+1) numberofX
                else numberoftokens m (i,j+1) mapsize numberofO (numberofX+1)
        else if (j == mapsize)
            then if (i < mapsize) 
                then numberoftokens m (i+1,0) mapsize numberofO numberofX
                else (numberofO,numberofX)
            else (numberofO,numberofX)

{-Realiza o movimento no tabuleiro-}
makemove :: Reversi -> (Int,Int) -> Int -> IO Reversi
makemove (Reversi b m) indice token = return (Reversi (Map.insert indice (player token) b) m)


{- --------------------------------------------
   Funções de manipulação da lista do jogo
   ---------------------------------------------}

{-Retorna o lista de movimentos possíveis-}
getMoveslist :: Reversi -> [(Int,Int)]
getMoveslist (Reversi b m) = m

{-Verifica se o tiro do usuário é válido-}
checkMove :: Reversi -> (Int, Int) -> Bool
checkMove rv@(Reversi b m) coord = do
    if(elem coord m) 
        then True
        else False
  
{-Pega uma tupla do usuário-}
getMove :: Reversi -> Int -> Int -> IO Reversi
getMove rv@(Reversi b m) mapsize player = do
      putStrLn ("Seleciona as coordenadas do seu próximo movimento\n")
      putStrLn ("Eixo x: de 0 a " ++ (show (mapsize-1)))
      x <- (getOpt mapsize)
      putStrLn ("Eixo y: de 0 a " ++ (show (mapsize-1)))
      y <- (getOpt mapsize)
      {-Verifica se o movimento é válido-}
      if (checkMove rv (makeTuple x y))
        then (makemove rv (makeTuple x y) player)
        else do 
          putStrLn ("Movimento inválido. Selecione outro.")
          getMove rv mapsize player
      
  

{- --------------------------------------------
   Funções de controle do jogo
   ---------------------------------------------}

{-Verifica se o jogo acabou-}
checkEnd :: Map (Int,Int) Char -> Int -> Bool
checkEnd m mapsize = do
    if ((numberofO == 0) || (numberofX == 0) || ((numberofO + numberofX) >= (mapsize*mapsize)))
        then True
        else False
    where (numberofO, numberofX) = (numberoftokens m (0,0) mapsize 0 0)
