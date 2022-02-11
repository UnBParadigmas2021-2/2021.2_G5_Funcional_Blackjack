-- import System.Random

module Blackjack (deck) where


-- Criando baralho

suits :: [[Char]]
suits = ["Hearts", "Diamonds", "Clubs", "Spades"]
values :: [Char]
values = ['1'..'9'] ++ ['A', 'J', 'Q', 'K']

deck :: [([Char], Char)]
deck = [(x, y) | x <- suits, y <- values]

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
