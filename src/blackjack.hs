module Blackjack (deck, shuffle) where
import System.Random
import Data.Array.IO
import Control.Monad

suits :: [[Char]]
suits = ["Hearts", "Diamonds", "Clubs", "Spades"]
values :: [Char]
values = ['1'..'9'] ++ ['A', 'J', 'Q', 'K']

deck :: [([Char], Char)]
deck = [(x, y) | x <- suits, y <- values]


-- Função retirada do site: 
-- https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs