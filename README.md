# Reversi - Haskell
# Descrição
Reversi ou Othello é um jogo de tabuleiro de estratégia para duas pessoas jogado em um tabuleiro 8 × 8, que geralmente é verde. Existem 64 blocos de jogo idênticos chamados discos, e eles são brilhantes de um lado e escuros do outro. Os jogadores se revezam para colocar as peças de xadrez no tabuleiro, com a cor designada voltada para cima. Em uma rodada, qualquer peça da cor do oponente em uma linha reta (horizontal, vertical ou diagonal) e restrita pelo disco recém-colocado e outro disco da cor do jogador atual será restaurado (clicado) para a cor do jogador atual.

# Regras
 A partida começa com 4 peças no centro do tabuleiro, duas brancas e duas pretas, sendo sempre as brancas na grande diagonal. O jogador com as pretas começa a partida colocando uma peça em uma das quatro opções marcadas no tabuleiro ao lado. Uma vez que um jogador faça uma jogada válida, todas as peças do oponente que estejam em uma linha reta (horizontal, vertical ou diagonal) entre a peça recém-colocada e qualquer outra peça do jogador que fez o movimento são viradas e passam a ser da cor deste jogador. 
 
 O jogo termina quando nenhum dos jogadores puder fazer movimentos (por exemplo, quando todas as casas estiverem ocupadas ou todas as peças do tabuleiro estiverem com a mesma cor virada para cima. Se um jogador possuir mais peças que o adversário, ele vence. Se ambos os jogadores tiverem a mesma quantidade de peças, o jogo termina em empate.
 
 # Dependências
 Para instalar o jogo e compilar é necessário fazer a instação do Haskell na sua máquina. Se vocês estiver no Windows, consulte https://www.haskell.org/platform/. Se for Linux, apenas execute:
 > sudo apt-get update
 > sudo apt-get install ghc
 
 Para compilar o programa execute:
 > ghc main.hs
 
 Para executar basta clicar duas vezes no main.exe no caso de Windows ou em Linux:
 > ./main
 
 # Como jogar
 Para jogar, pressione a tecla enter para iniciar o jogo. O jogo não conta com uma inteligência artificial, portanto são dois players. Cada jogada é a vez de um jogador. Será exibido na tela quais são as coordenadas de onde podem ser colocadas peças no tebuleiro. Para jogar escolha uma coordenada, por exemplo (2,3). Digite 2 e pressione enter e 3 e pressione enter.
 
 
