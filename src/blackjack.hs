module Blackjack (startGame, doubleBet) where

-- Criando baralho

suits :: [[Char]]
suits = ["Hearts", "Diamonds", "Clubs", "Spades"]
values :: [Char]
values = ['1'..'9'] ++ ['A', 'J', 'Q', 'K']

deck :: [([Char], Char)]
deck = [(x, y) | x <- suits, y <- values]

startGame :: Integer
startGame = 2000 -- Implementar função de iniciar jogo

bet :: Int -> Int
bet valor = valor -- Implementar função de apostar

doubleBet :: Num a => a -> a
doubleBet bet = bet * 2

-- ToDo: Criar menu
  -- Inicar jogo
  -- Pedir carta
  -- Apostar
  -- Passar vez
  -- Parar
  -- Finalizar jogo
  -- (Opcional) Dobrar aposta
-- ToDo: Remover carta aleatoria e atualizar o valor do baralho
-- ToDo: Comparar valores das cartas do jogador com as carta da mesa
-- ToDo: Valor do baralho deve ser reiniciado a cada partida
-- ToDo: Adicionar possibilidade do jogador apostar valor do pote e atualizar o mesmo
-- ToDo: Jogador pode apostar normal, dobrar valor da aposta ou passar
-- ToDo: Jogador pode encerrar jogo no final da partida ou assim que falir
-- ToDo (Opcional): Valor de aposta pode ser variavel
-- ToDo (Opcional): Adicionar ASSCI Art para quando o jogo encerrar
-- ToDo (Opcional): Fazer front com Haskel Gloss 
