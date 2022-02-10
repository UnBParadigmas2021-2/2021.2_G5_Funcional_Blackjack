-- import System.Random

module Blackjack (startGame, bet, doubleBet, chooseWinner,deck) where


-- Criando baralho
chips = 1000

suits = ["Hearts", "Diamonds", "Clubs", "Spades"]
values = concat [['1'..'9'], ['A', 'J', 'Q', 'K']]

deck = [(x, y) | x <- suits, y <- values]

startGame :: Int
startGame = 2000 -- Implementar função de iniciar jogo


bet :: Int -> Int
bet valor = valor -- Implementar função de apostar

doubleBet :: Int -> Int
doubleBet bet = (bet * 2)

chooseWinner :: IO()
chooseWinner = putStrLn $ "Você venceu!" -- Implementar função de definir vencedor

-- ToDo: Criar menu
  -- Inicar jogo
  -- Pedir carta
  -- Apostar
  -- Passar vez
  -- Parar
  -- Finalizar jogo
  -- (Opcional) Dobrar aposta
-- ToDo: Remover carta aleatoria e atualizar o valor do baralho
  --Selecionar carta aleatória.
  --Atualizar Baralho.
-- ToDo: Comparar valores das cartas do jogador com as carta da mesa
-- ToDo: Valor do baralho deve ser reiniciado a cada partida
-- ToDo: Adicionar possibilidade do jogador apostar valor do pote e atualizar o mesmo
-- ToDo: Jogador pode apostar normal, dobrar valor da aposta ou passar
-- ToDo: Jogador pode encerrar jogo no final da partida ou assim que falir
-- ToDo (Opcional): Valor de aposta pode ser variavel
-- ToDo (Opcional): Adicionar ASSCI Art para quando o jogo encerrar
-- ToDo (Opcional): Fazer front com Haskel Gloss 
