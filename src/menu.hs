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
main = mainMenu 2000

mainMenu :: Int -> IO ()
mainMenu money = do
  if money <= 0
    then putStrLn "Você perdeu...\n"
    else do
      putStrLn "Menu Principal do Jogo: "
      putStrLn "1 - Iniciar partida"
      putStrLn "2 - Fechar\n"
      option <- getLine
      putStrLn $ "Valor " ++ show money
      case option of
        "1" -> startGameMenu money
        "2" -> putStrLn "O jogo serah finalizado...\n"

startGameMenu :: Int -> IO ()
startGameMenu money = do
  putStrLn "Escolha sua acao: "
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
      putStrLn $ "Apostando: " ++ show 10
      inGameMenu 10 (money -10) playerHand dealerHand _deckShuffled counter
    "2" -> do
      putStrLn $ "Apostando: " ++ show 50
      inGameMenu 50 (money - 50) playerHand dealerHand _deckShuffled counter
    "3" -> do
      putStrLn $ "Apostando: " ++ show 100
      inGameMenu 100 (money - 100) playerHand dealerHand _deckShuffled counter
    "4" -> do
      putStrLn $ "Apostando: " ++ show 250
      inGameMenu 250 (money - 250) playerHand dealerHand _deckShuffled counter
    "5" -> do
      putStrLn $ "Apostando: " ++ show 500
      inGameMenu 500 (money - 500) playerHand dealerHand _deckShuffled counter
    "6" -> do
      putStrLn "Voltando para o menu...\n"
      mainMenu money

inGameMenu :: Int -> Int -> [([Char], Char)] -> [([Char], Char)] -> [([Char], Char)] -> Int -> IO ()
inGameMenu bet totalMoney playerHand dealerHand deckShuffled counter = do
  putStrLn $ "sua mão " ++ show playerHand
  putStrLn $ "mão do dealer " ++ show dealerHand

  compareHandValues playerHand dealerHand bet totalMoney

  putStrLn "Escolha sua acao: "
  putStrLn "1 - Dobrar aposta"
  putStrLn "2 - Comprar carta"
  putStrLn "3 - Fechar mao\n"
  option <- getLine

  case option of
    "1" -> do
      putStrLn $ "Dobrando aposta... Valor atual: " ++ show (bet * 2)
      let _playerHand = (deckShuffled !! (counter + 1)) : playerHand
      let _deckShuffled = drop (counter + 1) deckShuffled

      inGameMenu (bet * 2) (totalMoney - bet) _playerHand dealerHand _deckShuffled (counter + 1)
    "2" -> do
      putStrLn $ "Valor Total: " ++ show totalMoney

      let _playerHand = (deckShuffled !! (counter + 1)) : playerHand
      let _deckShuffled = drop (counter + 1) deckShuffled

      inGameMenu bet totalMoney _playerHand dealerHand _deckShuffled (counter + 1)
    "3" -> do
      putStrLn "Fechando mao...\n"

      let _dealerHand = (deckShuffled !! (counter + 1)) : dealerHand
      let _deckShuffled = drop (counter + 1) deckShuffled

      compareHandValues playerHand _dealerHand bet totalMoney

      mainMenu totalMoney

compareHandValues :: [([Char], Char)] -> [([Char], Char)] -> Int -> Int -> IO ()
compareHandValues hand dealerHand bet totalMoney = do
  let handValue = getHandValue hand
      handValueDealer = getHandValue dealerHand
  putStrLn $ "Valor da sua mão " ++ show handValue
  if handValue > 21
    then do
      putStrLn "Dealer venceu\n"
      mainMenu totalMoney
      exitSuccess
    else
      if handValueDealer > 21
        then do
          putStrLn "Você venceu\n"
          putStrLn $ "Seu dinheiro " ++ show (totalMoney + (2 * bet)) ++ "\n"
          mainMenu (totalMoney + (2 * bet))
          exitSuccess
        else do
          putStrLn ""

getHandValue :: [(a, Char)] -> Int
getHandValue = foldr ((+) . assignValueToCard) 0

assignValueToCard :: (a, Char) -> Int
assignValueToCard x = do
  let bool = isFamilyCard (snd x)
  case bool of
    True -> 10
    False -> digitToInt (snd x)

isFamilyCard :: Char -> Bool
isFamilyCard x
  | x == 'A' = True
  | x == 'J' = True
  | x == 'Q' = True
  | x == 'K' = True
  | otherwise = False
