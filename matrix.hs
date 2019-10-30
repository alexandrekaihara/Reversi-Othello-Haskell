module Matrix where


import Data.List    


-- :set +m   Serve para habilitar multiplas linhas no interpretador


-- Cria uma matrix x linhas e y colunas inicializada com z
matrix_create :: Int -> Int -> a -> [[a]];
matrix_create x y z = replicate y (replicate x z);


-- Retorna o elemento da linha x e coluna y
element :: [[element]] -> Int -> Int -> element;    
element matrix x y = matrix!!x!!y;


-- Retorna uma lista de elementos da linha x da matrix
line :: [[element]] -> Int -> [element];            
line matrix x = (++) [element matrix 0 x] 
    (if (length (tail matrix) /= 0) 
        then (line (tail matrix) x) 
        else []);


-- Retorna uma lista de elementos da coluna x da matrix
column :: [[element]] -> Int -> [element];          
column matrix y = matrix!!y;


-- Muda o valor de um elemento na matriz
--set_element :: [[element]] -> Int -> Int -> element -> [[element]]
--set_element matrix x y a = element matrix x y = a 


-- Printa a matriz 
--{
--matrix_print :: [[element]] -> IO();
--matrix_print matrix = do
--    {
--        putStrLn "   ";
        --print 0:(length matrix)
--    }
--}





