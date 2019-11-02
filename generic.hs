module Generic where 

import Data.Char 
import System.Random	
import Data.Map (Map, (!))
import qualified Data.Map as Map


{- --------------------------------------------
   Definições para o jogo
   ---------------------------------------------}

	{-Caracter que representa as fichas do jogador-}
	tokens :: Int -> Char
	tokens 0 = 'O'
	tokens 1 = 'X'
	tokens 2 = ' '


{- --------------------------------------------
   Funções genéricas de manipulação
   ---------------------------------------------}

	{-Lê uma string para um tupla de Int-}
	readTuple :: String -> (Int,Int)
	readTuple s = read s::(Int,Int)

	{-Tail seguro, funciona para string vazia-}
	myTail :: [a] -> [a]
	myTail s
		| null s    = []
		| otherwise = tail s

	{-String de número em 2 caracteres-}
	show2Dig :: Int -> String
	show2Dig n
		| n < 10    = "│ " ++ (show n)
		| otherwise = show n

	{-Imprime as linhas de duas strings simultaneamente-}
	printBoth :: String -> String -> Int -> String
	printBoth _  _ 10 = [] 
	printBoth [] [] n = []
	printBoth s1 s2 n =
		((lines s1) !! n) ++ " " ++ ((lines s2) !! n) ++ "\n"
			++ printBoth s1 s2 (n+1)

	{-Função que transforma uma lista de tuplas em uma 
	string contendo as tuplas-}
	giveKeys :: [(Int,Int)] -> String
	giveKeys [] = []
	giveKeys t  = (show (head t)) ++  " " ++ giveKeys (tail t)

	{- Transforma dois caracteres em uma tupla-}
	coordinate :: Char -> Char -> IO ((Int,Int))
	coordinate x y = return ((digitToInt x)+1, (digitToInt y)+1)







