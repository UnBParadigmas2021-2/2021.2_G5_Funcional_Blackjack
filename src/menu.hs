-- import Blackjack
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Blackjack
import Control.Monad
import Data.Char
import Data.Typeable
import System.Exit
import System.IO
import System.Random


main :: IO ()
main = do
  putStrLn "Bem-vindo ao BlackJack\n"
  mainMenu 2000

mainMenu :: Int -> IO ()
mainMenu money = do
  if money <= 0
    then putStrLn "Você faliu...\n"
    else do
      putStrLn "Menu Principal do Jogo: "
      putStrLn "1 - Iniciar partida"
      putStrLn "2 - Fechar\n"
      option <- getLine
      case option of
        "1" -> startGameMenu money
        "2" -> putStrLn "O jogo serah finalizado...\n"

startGameMenu :: Int -> IO ()
startGameMenu money = do
  putStrLn $ "Seu dinheiro: $ " ++ show money
  putStrLn "\nEscolha sua acao: "
  putStrLn "1 - Apostar 10"
  putStrLn "2 - Apostar 50"
  putStrLn "3 - Apostar 100"
  putStrLn "4 - Apostar 250"
  putStrLn "5 - Apostar 500"
  putStrLn "6 - Voltar para o menu principal\n"

  option <- getLine
  deckShuffled <- shuffle deck
  let playerHand = head deckShuffled : [deckShuffled !! 1]
  let dealerHand = [deckShuffled !! 2]
  let counter = 3
  let _deckShuffled = drop counter deckShuffled

  case option of
    "1" -> do
      if (money - 10) < 0
        then do
          putStrLn "Dinheiro insuficiente para a aposta minima. Voltando para o menu principal..." 
          mainMenu money
      else do
        putStrLn $ "Apostando: " ++ show 10
        inGameMenu 10 (money -10) playerHand dealerHand _deckShuffled
    "2" -> do
      if (money - 50) < 0
        then do
          putStrLn "Dinheiro insuficiente!" 
          startGameMenu money
      else do
        putStrLn $ "Apostando: " ++ show 50
        inGameMenu 50 (money - 50) playerHand dealerHand _deckShuffled
    "3" -> do
      if (money - 100) < 0
        then do
          putStrLn "Dinheiro insuficiente!" 
          startGameMenu money
      else do
        putStrLn $ "Apostando: " ++ show 100
        inGameMenu 100 (money - 100) playerHand dealerHand _deckShuffled
    "4" -> do
      if (money - 250) < 0
        then do
          putStrLn "Dinheiro insuficiente!" 
          startGameMenu money
      else do
        putStrLn $ "Apostando: " ++ show 250
        inGameMenu 250 (money - 250) playerHand dealerHand _deckShuffled
    "5" -> do
      if (money - 500) < 0
        then do
          putStrLn "Dinheiro insuficiente!" 
          startGameMenu money
      else do
        putStrLn $ "Apostando: " ++ show 500
        inGameMenu 500 (money - 500) playerHand dealerHand _deckShuffled
    "6" -> do
        putStrLn "Voltando para o menu...\n"
        mainMenu money

inGameMenu :: Int -> Int -> [([Char], Char)] -> [([Char], Char)] -> [([Char], Char)] -> IO ()
inGameMenu bet totalMoney playerHand dealerHand deckShuffled = do
  putStrLn $ "\nSua mão:\n" ++ (printHand playerHand)
  putStrLn $ "Mão do dealer:\n" ++ (printHand dealerHand)

  compareHandValuesOverOrEqual21 playerHand dealerHand bet totalMoney
  putStrLn $ "Seu dinheiro: $ " ++ show totalMoney
  putStrLn $ "\n----------------------------------------\n" ++ "Escolha sua acao: "
  putStrLn "1 - Dobrar aposta"
  putStrLn "2 - Comprar carta"
  putStrLn "3 - Fechar mao\n"
  option <- getLine

  case option of
    "1" -> do
      if (totalMoney - bet) < 0
        then do
          putStrLn "Dinheiro insuficiente para dobrar a aposta"
          inGameMenu bet totalMoney playerHand dealerHand deckShuffled
        else do
          putStrLn $ "Dobrando aposta... Valor atual: " ++ show (bet * 2)
          let _playerHand = head deckShuffled : playerHand
          inGameMenu (bet * 2) (totalMoney - bet) _playerHand dealerHand (drop 1 deckShuffled)
    "2" -> do
      let _playerHand = head deckShuffled : playerHand

      let handValuePlayer = getHandValue _playerHand
      let handValueDealer = getHandValue dealerHand
      if handValueDealer < handValuePlayer
        then do
          let _dealerHand = head (drop 1 deckShuffled) : dealerHand
          inGameMenu bet totalMoney _playerHand _dealerHand (drop 2 deckShuffled)
        else do
          inGameMenu bet totalMoney _playerHand dealerHand (drop 1 deckShuffled)

    "3" -> do
      putStrLn "Fechando mao...\n"

      let handValuePlayer = getHandValue playerHand
      let handValueDealer = getHandValue dealerHand
      if handValueDealer < handValuePlayer
        then do
          let _dealerHand = head deckShuffled : dealerHand
          compareHandValuesOverOrEqual21 playerHand _dealerHand bet totalMoney
          compareHandValues playerHand _dealerHand bet totalMoney
          mainMenu totalMoney
        else do
          compareHandValuesOverOrEqual21 playerHand dealerHand bet totalMoney
          compareHandValues playerHand dealerHand bet totalMoney
          mainMenu totalMoney

compareHandValuesOverOrEqual21 :: [([Char], Char)] -> [([Char], Char)] -> Int -> Int -> IO ()
compareHandValuesOverOrEqual21 hand dealerHand bet totalMoney = do
  let handValue = getHandValue hand
      handValueDealer = getHandValue dealerHand
  putStrLn $ "Valor da sua mão " ++ show handValue
  putStrLn $ "Valor da mão do dealer " ++ show handValueDealer
  if handValue > 21 || handValueDealer == 21
    then do
      putStrLn "Dealer venceu\n"
      mainMenu totalMoney
      exitSuccess
    else
      if handValueDealer > 21 || handValue == 21
        then do
          putStrLn "Você venceu\n"
          putStrLn $ "Seu dinheiro: $ " ++ show (totalMoney + (2 * bet)) ++ "\n"
          mainMenu (totalMoney + (2 * bet))
          exitSuccess
        else do
          putStrLn ""

compareHandValues :: [([Char], Char)] -> [([Char], Char)] -> Int -> Int -> IO ()
compareHandValues hand dealerHand bet totalMoney = do
  let handValue = getHandValue hand
      handValueDealer = getHandValue dealerHand
  if handValueDealer > handValue
    then do
      putStrLn "Dealer venceu\n"
      mainMenu totalMoney
      exitSuccess
    else
      if handValue > handValueDealer
        then do
          putStrLn "Você venceu\n"
          putStrLn $ "Seu dinheiro: $ " ++ show (totalMoney + (2 * bet)) ++ "\n"
          mainMenu (totalMoney + (2 * bet))
          exitSuccess
        else do
          putStrLn "Empate\n"
          putStrLn $ "Seu dinheiro: $ " ++ show (totalMoney + bet) ++ "\n"
          mainMenu (totalMoney + bet)
          exitSuccess

getHandValue :: [(a, Char)] -> Int
getHandValue = foldr ((+) . assignValueToCard) 0

assignValueToCard :: (a, Char) -> Int
assignValueToCard x = do
  case (snd x) of
    y | y == 'J' || y == 'Q' || y == 'K' || y == 'X' -> 10
    'A' -> 1
    _ -> digitToInt (snd x)
