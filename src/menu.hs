import System.IO
import System.Random
import Control.Monad
import Blackjack
import Data.Typeable


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
      putStrLn $ "Valor" ++ show money
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
      inGameMenu 10 (money -10)
    "2" -> do
      putStrLn $ "Apostando: " ++ show 50
      inGameMenu 50 (money - 50)
    "3" -> do
      putStrLn $ "Apostando: " ++ show 100
      inGameMenu 100 (money - 100)
    "4" -> do
      putStrLn $ "Apostando: " ++ show 250
      inGameMenu 250 (money - 250)
    "5" -> do
      putStrLn $ "Apostando: " ++ show 500
      inGameMenu 500 (money - 500)
    "6" -> do
      putStrLn "Voltando para o menu...\n"
      mainMenu money

inGameMenu :: Int -> Int -> IO ()
inGameMenu bet totalMoney = do
  putStrLn "Escolha sua acao: "
  putStrLn "1 - Dobrar aposta"
  putStrLn "2 - Comprar carta"
  putStrLn "3 - Fechar mao\n"
  option <- getLine
  case option of
    "1" -> do
      putStrLn $ "Dobrando aposta... Valor atual: " ++ show (bet * 2)
      inGameMenu (bet * 2) (totalMoney - bet)
    "2" -> do
      putStrLn $ "Valor Total: " ++ show totalMoney
      inGameMenu bet totalMoney
    "3" -> do
      putStrLn "Fechando mao...\n"
      mainMenu totalMoney


initialHand :: StdGen -> IO ()
initialHand genOne = do
    let (randOne, newGen) = randomR (1,52) genOne :: (Int, StdGen)
        (randTwo, anotherGen) = randomR (1,52) newGen :: (Int, StdGen)
        hand = [deck !! randOne] ++ [deck !! randTwo]
    putStrLn $ "Total da mão " ++ show((randOne `mod` 13) + (randTwo `mod` 13) + 2 ) ++ " " ++ show hand
    -- putStrLn $ "Total da mão " ++ show deck