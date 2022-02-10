import System.IO
import System.Random
import Control.Monad
import Blackjack
import Data.Typeable


main :: IO ()
main = mainMenu

mainMenu :: IO()
mainMenu = do{ 
    putStrLn $"Menu Principal do Jogo: ";
    putStrLn $"1 - Iniciar partida";
    putStrLn $"2 - Fechar";
    option <- getLine;
    case option of 
        "1" -> startGameMenu Blackjack.startGame;
        "2" -> putStrLn $"O jogo sera finalizado...";
}

startGameMenu :: Int -> IO()
startGameMenu money = do {    
    putStrLn $"Escolha sua acao: ";
    putStrLn $"1 - Apostar 10";
    putStrLn $"2 - Apostar 50";
    putStrLn $"3 - Apostar 100";
    putStrLn $"4 - Apostar 250";
    putStrLn $"5 - Apostar 500";
    putStrLn $"6 - Voltar para o menu principal";
    option <- getLine;
    case option of 
        "1" -> do
            putStrLn $ "Apostando: " ++ show 10
            inGameMenu(Blackjack.bet(10));
        "2" -> do
            putStrLn $ "Apostando: " ++ show 50
            inGameMenu(Blackjack.bet(50));
        "3" -> do
            putStrLn $ "Apostando: " ++ show 100
            inGameMenu(Blackjack.bet(100));
        "4" -> do
            putStrLn $ "Apostando: " ++ show 250
            inGameMenu(Blackjack.bet(250));
        "5" -> do
            putStrLn $ "Apostando: " ++ show 500
            inGameMenu(Blackjack.bet(500));
        "6" -> do
            putStrLn $"Voltando para o menu..."; 
            mainMenu;
}

inGameMenu :: Int -> IO()
inGameMenu bet = do {
    

    genOne <- getStdGen;  
    initialHand genOne;
    putStrLn $"fora da função: " ++ show hand;
    putStrLn $"Escolha sua acao: ";
    putStrLn $"1 - Dobrar aposta";
    putStrLn $"2 - Comprar carta";
    putStrLn $"3 - Fechar mao";
    option <- getLine;
    case option of 
        "1" -> do
            putStrLn $ "Dobrando aposta... Valor atual: " ++ show (bet * 2)
            inGameMenu(Blackjack.doubleBet(bet));
        "2" -> do
            putStrLn $"Comprando carta...";
            inGameMenu bet
        "3" -> do 
            putStrLn $"Fechando mao...";
            chooseWinner
            mainMenu
}

initialHand :: StdGen -> IO ()
initialHand genOne = do
    let (randOne, newGen) = randomR (1,52) genOne :: (Int, StdGen)
        (randTwo, anotherGen) = randomR (1,52) newGen :: (Int, StdGen)
        hand = [deck !! randOne] ++ [deck !! randTwo]
    putStrLn $ "Sua mão " ++ show hand