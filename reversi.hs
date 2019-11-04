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
    let caractere = [player 1, player 0, player 0, player 1] 
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
   Funções de manipulação da lista de movimentações
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
    print m
    putStrLn ("Seleciona as coordenadas (player "++ (show player) ++ ")\n")
    putStrLn ("Eixo x: de 0 a " ++ (show (mapsize-1)))
    x <- (getOpt mapsize)
    putStrLn ("Eixo y: de 0 a " ++ (show (mapsize-1)))
    y <- (getOpt mapsize)
    {-Verifica se o movimento é válido-}
    if (checkMove rv (makeTuple y x))
    then (makemove rv (makeTuple y x) player)
    else do 
        cleanScreen
        showBoard rv mapsize
        putStrLn ("Movimento inválido. Selecione outro.")
        getMove rv mapsize player

{-Retorna todas as coordenadas de todos os espaços vazios do tabuleiro
  OBS: valor da coordenada inicial deve ser (0,0)-}
getAllBlanks :: Map (Int, Int) Char -> Int-> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getAllBlanks m mapsize t@(i,j) lista
    | j < mapsize = if (Map.notMember t m)
            then getAllBlanks m mapsize (i,j+1) (lista ++ [t])
            else getAllBlanks m mapsize (i,j+1) lista
    | i < mapsize - 1 = do
        getAllBlanks m mapsize (i+1,0) lista
    | otherwise   = lista

{-Retorna uma tupla com a direção especificada-}
next :: (Int, Int) -> Int -> (Int, Int)
next t@(i,j) direction
    | direction == 0 = (i-1,j-1)
    | direction == 1 = (i-1,j  )
    | direction == 2 = (i-1,j+1)
    | direction == 3 = (i  ,j+1)
    | direction == 4 = (i+1,j+1)
    | direction == 5 = (i+1,j  )
    | direction == 6 = (i+1,j-1)
    | direction == 7 = (i  ,j-1)
    | otherwise = t   

{-Dado um espaço vazio no tabuleiro, retorna um bool se pode ser realizado um movimento
  expandindo recursivamente em todas as direções-}
analyzeBlank :: Map (Int, Int) Char -> Int -> Int -> (Int,Int) -> (Int,Int) -> Int -> Bool
analyzeBlank m mapsize token t@(i,j) t2@(i1,j1) direction
    | (i < 0 || j < 0 || i >= mapsize || j >= mapsize) = analyzeBlank m mapsize token t t (direction+1)
    | direction < 8 = 
        if ((Map.member (next t2 direction) m) && (m ! (next t2 direction)) /= (player token))  
            then analyzeBlank m mapsize token t (next t2 direction) direction    
            else if (Map.notMember t2 m)                              
                then analyzeBlank m mapsize token t t (direction+1)
                else if ((Map.member (next t2 direction) m) && (m ! (next t2 direction)) == (player token))
                    then True
                    else analyzeBlank m mapsize token t t (direction+1)    
    | otherwise = False

{-Retorna um reversi com todos os movimentos possíveis no mapa-}
findMoves :: Reversi -> Int -> Int -> [(Int, Int)] -> IO Reversi
findMoves rv@(Reversi b m) mapsize token lista@(x:xs)
    | xs == [] = return(Reversi b m)
    | analyzeBlank b mapsize token x x 0 = findMoves (Reversi b (m ++ [x])) mapsize token xs
    | otherwise =  findMoves (Reversi b m) mapsize token xs


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

{-Executa a função principal de loop-}
playRV :: Reversi -> Int -> Int -> Int -> IO()
playRV rv@(Reversi b m) mapsize player twoplayer = do
	{-Verifica se o jogo acabou-}
    if (checkEnd b mapsize)
        then do
            cleanScreen
            showBoard rv mapsize
            if (player == 0)
                then showVictory
                else showDefeat
            return ()
        {-Iterador loop dos jogadores 0 e 1-}
        else if (player == 0 || twoplayer == 1)
            {-Se for o turno do jogador 0-}
            then do
                cleanScreen                                                                             {-Limpa a tela-}
                rv1 <- (findMoves (Reversi b []) mapsize player (getAllBlanks b mapsize (0,0) []))      {-Descobre uma lista de movimentos disponíveis-}
                showBoard rv1 mapsize                                                                   {-Printa o tabuleiro no terminal-}
                rv2 <- (getMove  rv1 mapsize player)                                                    {-Recebe um par ordenado como jogada-}
                {-Inserir a função que muda as peças no mapa de acordo com o movimento escolhido-}
                playRV rv2 mapsize (abs(player - 1)) twoplayer
            {-Se for o turno da IA-}
            else do
                cleanScreen
                showBoard rv mapsize
                rv2 <- (getMove rv mapsize player)
                playRV rv2 mapsize (abs(player - 1)) twoplayer
