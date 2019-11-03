module Generic where 

import Data.Char 
import System.Random
import Data.Map (Map, (!))
import qualified Data.Map as Map


{- --------------------------------------------
   Definições para o jogo
   ---------------------------------------------}

{-Caracter que representa as fichas do jogador-}
player :: Int -> Char
player 0 = 'O'
player 1 = 'X'


{- --------------------------------------------
   Funções genéricas de manipulação
   ---------------------------------------------}

{-Lê uma string para um tupla de Int-}
readTuple :: String -> (Int,Int)
readTuple s = read s::(Int,Int)

{-Tail seguro, funciona para string vazia-}
myTail :: [a] -> [a]
myTail s
	| null s = []
	| otherwise = tail s

{-String de número em 2 caracteres-}
show2Dig :: Int -> String
show2Dig n = "│ " ++ (show n)

{-Imprime as linhas das linhas de strings simultaneamente-}
printlines :: String -> Int -> Int-> String
printlines [] n mapsize = []
printlines s1 n mapsize = ((lines s1) !! n) ++ "\n" ++ 
	(if(n+1 < mapsize)
		then printlines s1 (n+1) mapsize
		else "")
	

{-Função que transforma uma lista de tuplas em uma 
string contendo as tuplas-}
giveKeys :: [(Int,Int)] -> String
giveKeys [] = []
giveKeys t  = (show (head t)) ++  " " ++ giveKeys (tail t)

{- Transforma dois caracteres em uma tupla-}
coordinate :: Char -> Char -> IO ((Int,Int))
coordinate x y = return ((digitToInt x)+1, (digitToInt y)+1)







