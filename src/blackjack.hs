module Blackjack

-- Criando baralho
chips = 1000

suits = ["Hearts", "Diamonds", "Clubs", "Spades"]
values = concat [['1'..'9'], ['A', 'J', 'Q', 'K']]

deck = [(x, y) | x <- suits, y <- values]

-- ToDo: Remover carta aleatoria e atualizar o valor do baralho 
-- ToDo: Valor do baralho deve ser reiniciado a cada partida
-- ToDo: Adicionar possibilidade do jogador apostar valor do pote e atualizar o mesmo
-- ToDo: Jogador pode apostar normal, dobrar valor da aposta ou passar
-- ToDo: Jogador pode escerrar jogo no final da partida ou assim que falir
-- ToDo (Opcional): Valor de aposta pode ser variavel
-- ToDo (Opcional): Adicionar ASSCI Art para quando o jogo encerrar
