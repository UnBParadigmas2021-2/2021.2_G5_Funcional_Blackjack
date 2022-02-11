import System.IO
import System.Random
import Control.Monad
import Blackjack
import Data.Typeable
import Data.Char
import System.Exit


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

startGameMenu money = do
  putStrLn "Escolha sua acao: "
  putStrLn "1 - Apostar 10"
  putStrLn "2 - Apostar 50"
  putStrLn "3 - Apostar 100"
  putStrLn "4 - Apostar 250"
  putStrLn "5 - Apostar 500"
  putStrLn "6 - Voltar para o menu principal\n"
  option <- getLine
  case option of
    "1" -> do
      putStrLn $ "Apostando: " ++ show 10
      let hand = initialHand (mkStdGen 2021)
          dealerHand = initialHand (mkStdGen 1999)
      inGameMenu 10 (money -10) hand dealerHand
    "2" -> do
      putStrLn $ "Apostando: " ++ show 50
      let hand = initialHand (mkStdGen 2021)
          dealerHand = initialHand (mkStdGen 1999)
      inGameMenu 50 (money - 50) hand dealerHand
    "3" -> do
      putStrLn $ "Apostando: " ++ show 100
      let hand = initialHand (mkStdGen 2021)
          dealerHand = initialHand (mkStdGen 1999)
      inGameMenu 100 (money - 100) hand dealerHand
    "4" -> do
      putStrLn $ "Apostando: " ++ show 250
      let hand = initialHand (mkStdGen 2021)
          dealerHand = initialHand (mkStdGen 1999)
      inGameMenu 250 (money - 250) hand dealerHand
    "5" -> do
      putStrLn $ "Apostando: " ++ show 500
      let hand = initialHand (mkStdGen 2021)
          dealerHand = initialHand (mkStdGen 1999)
      inGameMenu 500 (money - 500) hand dealerHand
    "6" -> do
      putStrLn "Voltando para o menu...\n"
      mainMenu money

inGameMenu :: Int -> Int -> [([Char], Char)] -> [([Char], Char)] -> IO ()
inGameMenu bet totalMoney hand dealerHand = do
  putStrLn $ "sua mão " ++ show hand
  putStrLn $ "mão do dealer " ++ show dealerHand
  compareHandValues hand dealerHand bet totalMoney
  putStrLn "Escolha sua acao: "
  putStrLn "1 - Dobrar aposta"
  putStrLn "2 - Comprar carta"
  putStrLn "3 - Fechar mao\n"
  option <- getLine
  case option of
    "1" -> do
      putStrLn $ "Dobrando aposta... Valor atual: " ++ show (bet * 2)
      inGameMenu (bet * 2) (totalMoney - bet) hand dealerHand
    "2" -> do
      putStrLn $ "Valor Total: " ++ show totalMoney
      inGameMenu bet totalMoney hand dealerHand
    "3" -> do
      putStrLn "Fechando mao...\n"
      mainMenu totalMoney


initialHand :: StdGen -> [([Char], Char)]
initialHand genOne = do
    let (randOne, newGen) = randomR (1,52) genOne :: (Int, StdGen)
        (randTwo, anotherGen) = randomR (1,52) newGen :: (Int, StdGen)
        hand = [deck !! randOne] ++ [deck !! randTwo]
    --putStrLn $ "Total da mão " ++ show((randOne `mod` 13) + (randTwo `mod` 13) + 2 ) ++ " " ++ show hand
    return hand !! 0
    -- putStrLn $ "Total da mão " ++ show deck

compareHandValues :: [([Char], Char)] -> [([Char], Char)] -> Int -> Int -> IO ()
compareHandValues hand dealerHand bet totalMoney = do
    let handValue = getHandValue hand
        handValueDealer = getHandValue dealerHand
    putStrLn $ "Valor da sua mão " ++ show handValue
    if handValue > 21 then do
      putStrLn "Dealer venceu\n"
      mainMenu totalMoney
      exitSuccess
    else if handValueDealer > 21 then do
      putStrLn "Você venceu\n"
      putStrLn $ "Seu dinheiro " ++ show (totalMoney + (2*bet)) ++ "\n"
      mainMenu (totalMoney + (2*bet))
      exitSuccess
    else do
      putStrLn ""
  

getHandValue :: [([Char], Char)] -> Int
getHandValue [] = 0
getHandValue (h:t) = assignValueToCard h + getHandValue t 

assignValueToCard :: ([Char], Char) -> Int
assignValueToCard x = do
    let bool = isFamilyCard (snd x)
    case bool of
        True -> 10
        False -> digitToInt (snd x)

isFamilyCard :: Char -> Bool
isFamilyCard x  | x == 'A' = True
                | x == 'J' = True
                | x == 'Q' = True
                | x == 'K' = True
                | otherwise = False


