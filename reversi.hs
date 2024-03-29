module Reversi where

import Generic
import Data.Char
import Interaction
import Data.Map (Map, (!))
import qualified Data.Map as Map


{-Representação do jogo: Tabuleiro e duas listas de movimentos possíveis.-}
data Reversi = Reversi (Board) Moves deriving (Show)

{-Tabuleiro das peças    do usuário-}
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
    let caractere = [token 1, token 0, token 0, token 1] 
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
moveInsert :: Reversi -> (Int,Int) -> Int -> IO Reversi
moveInsert (Reversi b m) indice player = return (Reversi (Map.insert indice (token player) b) m)

{-Realiza o movimento no tabuleiro-}
mapUpdate :: Reversi -> (Int,Int) -> Int -> Reversi
mapUpdate (Reversi b m) indice player = Reversi (Map.insert indice (token player) b) m

{-Muda as fichas que estejam entre duas coordenadas-}
changetokensline :: Reversi -> Int -> (Int, Int) -> (Int, Int) -> Int -> IO Reversi
changetokensline rv@(Reversi b m) player t t2@(i,j) direction
    | t /= t2   = changetokensline (mapUpdate rv t player) player (next t direction) t2 direction
    | otherwise = return (rv)

{-Aplica as mudanças de peças no tabuleiro de acordo com o movimento realizado-}
changetokens :: Reversi -> Int -> Int -> (Int, Int) -> (Int, Int) -> Int -> IO Reversi
changetokens rv@(Reversi b m) mapsize player t@(i,j) t2@(i1,j1) direction
    | (i1 < 0 || j1 < 0 || i1 >= mapsize || j1 >= mapsize) = changetokens rv mapsize player t t (direction+1)
    | direction < 8 = 
        if ((Map.member (next t2 direction) b) && (b ! (next t2 direction)) /= (token player))  
            then changetokens rv mapsize player t (next t2 direction) direction    
            else if ((Map.member (next t2 direction) b) && (b ! (next t2 direction)) == (token player))
                then do               
                    newrv <- (changetokensline rv player t (next t2 direction) direction)     
                    changetokens newrv mapsize player t t (direction+1)
                else changetokens rv mapsize player t t (direction+1)    
    | otherwise = return(rv)

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
getMove :: Reversi -> Int -> Int -> IO (Int, Int)
getMove rv@(Reversi b m) mapsize player = do
    putStrLn ("Seleciona as coordenadas (player "++ (show player) ++ ")\n")
    putStr "Movimentos possiveis: " 
    print m
    putStrLn "Eixo x: "  
    x <- (getOpt mapsize)
    putStrLn "Eixo y: "
    y <- (getOpt mapsize)
    {-Verifica se o movimento é válido-}
    if (checkMove rv (makeTuple x y))
        then return (makeTuple x y)
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
analyzeBlank m mapsize player t@(i,j) t2@(i1,j1) direction
    | (i1 < 0 || j1 < 0 || i1 >= mapsize || j1 >= mapsize) = analyzeBlank m mapsize player t t (direction+1)
    | direction < 8 = do
        let vizinho = (next t2 direction)
        if ((Map.member vizinho m) && (m ! vizinho) /= (token player))  
            then analyzeBlank m mapsize player t vizinho direction    
            else if (Map.notMember t2 m)                              
                then analyzeBlank m mapsize player t t (direction+1)
                else if ((Map.member vizinho m) && (m ! vizinho) == (token player))
                    then True
                    else analyzeBlank m mapsize player t t (direction+1)    
    | otherwise = False

{-Retorna um reversi com todos os movimentos possíveis no mapa-}
findMoves :: Reversi -> Int -> Int -> [(Int, Int)] -> IO Reversi
findMoves rv@(Reversi b m) mapsize player lista@(x:xs)
    | (xs == []) = return(Reversi b m)
    | analyzeBlank b mapsize player x x 0 = findMoves (Reversi b (m ++ [x])) mapsize player xs
    | otherwise =  findMoves (Reversi b m) mapsize player xs

{- --------------------------------------------
   Funções de controle do jogo
   ---------------------------------------------}

{-Verifica se o jogo acabou-}
checkEnd :: Reversi -> Int -> Int -> Bool
checkEnd rv@(Reversi b m) mapsize outmoves = do
    if ((numberofO == 0) || (numberofX == 0) || ((numberofO + numberofX) >= (mapsize*mapsize)) || outmoves == 2)
        then True
        else False
    where   (numberofO, numberofX) = (numberoftokens b (0,0) mapsize 0 0)

{- --------------------------------------------
   Funções da IA
   ---------------------------------------------}
{-
{-Verifica de que lado esta a coordenada (x, y) no tabuleiro-}
pontuacao m player x y {-Isso era pra retornar um inteiro-}
    | (( m ! (x, y)) == 'O' && player == 0) = 1
    | (( m ! (x, y)) == 'O' && player == 1) = -1
    | (( m ! (x, y)) == 'X' && player == 0) = -1
    | (( m ! (x, y)) == 'X' && player == 1) = 1
    | otherwise = 0

{-Adiciona um valor com peso à soma que indica se a posição atual do tabuleiro é boa ou ruim-}
evaluate_v b mapsize player x y {-Isso era pra retornar um inteiro-}
    | (x == 0 || x == (mapsize - 1)) && (y == 0 || y == (mapsize - 1)) = 25*(pontuacao b player x y)
    | (x == 0 || x == 1 || x == (mapsize - 1) || x == (mapsize -2 )) && (y == 1 || y == (mapsize - 2)) = -10*(pontuacao b player x y)
    | (x == 1 || x == (mapsize - 2)) && (y == 0 || y == (mapsize - 1)) = -10*(pontuacao b player x y)
    | (x == 0 || x == (mapsize - 1) || y == 0 || y == (mapsize - 1)) = 15*(pontuacao b player x y)
    | (y == 1 || y == (mapsize - 2) || x == 1 || x == (mapsize - 2)) = -2*(pontuacao b player x y)
    | otherwise = (pontuacao b player x y)

{-Percorre o mapa no eixo horizontal, formando uma soma que indica se a posição atual do tabuleiro é boa ou ruim-}
evaluate_h b mapsize player x {-Isso era pra retornar um inteiro-}
    | x == (mapsize - 1) = evaluate_v b mapsize player x 0 
    | otherwise = evaluate_v b mapsize player x 0 + evaluate_h b mapsize player x+1 
    
{-Funcao min da arvore minimax-}
mini b _ 6 mapsize player = evaluate_v b mapsize player 0 {-Isso era pra retornar um inteiro-}
mini b ((x, y):m) nivel mapsize player = do
    let newb = changetokens b mapsize player x y
        moves = getMove newb mapsize player
    return min (mini b m nivel mapsize player) (imax newb moves nivel+1 mapsize player)

{-Funcao max da arvore minimax-}
imax b _ 6 mapsize player = evaluate_v b mapsize player 0 {-Isso era pra retornar um inteiro-}
imax b ((x, y):m) nivel mapsize player = do
    let newb = changetokens b mapsize player x y
        moves = getMove newb mapsize player
    return max (imax b m nivel mapsize player) (mini newb moves nivel+1 mapsize player)


imax _ _ _ _ _ = 5

{-Inicializa a arvore minimax de cada uma das possibilidades de movimentos e retorna o indice do melhor-}
moveIndex :: Reversi -> Int -> Int -> Int -> (Int, Int)
moveIndex rv@(Reversi b []) _ _ _ = (-1000::Int, -1000::Int)
moveIndex rv@(Reversi b ((x, y):m)) mapsize player ind = do
    rv1 <- changetokens (Reversi b m) mapsize player (x, y) (x, y) 0
    let blanks = ((getAllBlanks b mapsize (0,0) []) ++ [(-1,-1)])  
        movimentos = (findMoves rv1 mapsize player blanks) 
        pr = moveIndex (Reversi b m) mapsize player (ind+1)                 
        at = imax rv1 movimentos 0 mapsize player
    if ( (fst pr) > at )
        then pr
        else (at, ind)

{-Escolhe o melhor movimento da IA dentro da lista de movimentos possiveis-}
getIAmove :: Reversi -> Int -> Int -> (Int, Int)
getIAmove rv@(Reversi b moves) mapsize player = do {-Isso era pra retornar um par de inteiros (x, y)-}
    let par = moves !! (snd (moveIndex rv mapsize player 0))
        x = show (fst par)
        y = show (snd par)
    (makeTuple x y)
-}

{- --------------------------------------------
   Função principal de loop do jogo
   ---------------------------------------------}
playRV :: Reversi -> Int -> Int -> Int -> Int -> IO()
playRV rv@(Reversi b m) mapsize player twoplayer outmoves = do
    cleanScreen                                                                 {-Limpa a tela                                 -}
    let blanks = ((getAllBlanks b mapsize (0,0) []) ++ [(-1,-1)])               {-Acha os espaços vazios no tabuleiro-}
    rv1@(Reversi b1 m1) <- (findMoves (Reversi b []) mapsize player blanks)     {-Descobre uma lista de movimentos disponíveis -}
    showBoard rv1 mapsize                                                       {-Printa o tabuleiro no terminal               -}
    {-Verifica se o jogo acabou por:          -}
    {-    -Sem movimento dos dois players;    -}
    {-   -Um player ficou sem fichas;         -}
    {-   -Tabuleiro foi totalmente preenchido;-}
    if (checkEnd rv1 mapsize outmoves)
        then do
            let (numberofO, numberofX) = (numberoftokens b (0,0) mapsize 0 0)
            if (numberofO >= numberofX)
                then showVictory
                else showDefeat
            return ()
        else if (player == 0 || twoplayer == 1)  
            then if (m1 /= [])
                {-Turno dos players-} 
                then do
                    move <- (getMove  rv1 mapsize player)                   {-Recebe um par ordenado como jogada           -}
                    rv2  <- changetokens rv1 mapsize player move move 0     {-Aplica o movimento e as mudanças no tabuleiro-}
                    playRV rv2 mapsize (abs(player - 1)) twoplayer 0
                else if (outmoves == 0)
                    then do
                        showOutOfMoves
                        getChar
                        let outmoves1 = 1
                        playRV rv1 mapsize (abs(player - 1)) twoplayer outmoves1
                    else do
                        let outmoves1 = 2
                        playRV rv1 mapsize (abs(player - 1)) twoplayer outmoves1
            else do
                {-IA-}
                print(5)
                {-let move = (getIAmove rv1 mapsize player)
                rv2 <- changetokens rv1 mapsize player move move 0
                playRV rv2 mapsize (abs(player - 1)) twoplayer 0
                cleanScreen-}
    
