import Matrix
import Reversi
import ReversiIA

-- Compiacao: ghc --make main.hs
-- :set +m   Serve para habilitar multiplas linhas no interpretador
main = do 
{
    let a = line (create 4 4 (0, ' ')) 3 
    in  print (a)
    
}
