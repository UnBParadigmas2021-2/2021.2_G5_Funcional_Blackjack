module Blackjack (deck, shuffle, printHand) where
import System.Random
import Data.Array.IO
import Control.Monad

suits :: [[Char]]
suits = ["Hearts", "Diamonds", "Clubs", "Spades"]
values :: [Char]
values = ['2'..'10'] ++ ['A', 'J', 'Q', 'K']

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

--Arte baseada no site
-- https://ascii.co.uk/art/cards
printHand :: [([Char], Char)] -> String
printHand [] = ""
printHand (h:t)
    | suit == "Hearts" =  ".------.\n\
                          \|" ++ [(snd h)] ++ "_  _ |\n\
                          \|( \\/ )|\n\
                          \| \\  / |\n\
                          \|  \\/ " ++ [(snd h)] ++"|\n\
                          \`------´\n" ++ (printHand t)
    | suit == "Diamonds" =  ".------.\n\
                            \|" ++ [(snd h)] ++ " /\\  |\n\
                            \| /  \\ |\n\
                            \| \\  / |\n\
                            \|  \\/ " ++ [(snd h)] ++"|\n\
                            \`------´\n" ++ (printHand t)
    | suit == "Clubs" =  ".------.\n\
                        \|" ++ [(snd h)] ++ " _   |\n\
                        \| ( )  |\n\
                        \|(_x_) |\n\
                        \|  Y  " ++ [(snd h)] ++"|\n\
                        \`------´\n" ++ (printHand t)
    | suit == "Spades" =  ".------.\n\
                          \|" ++ [(snd h)] ++ " .   |\n\
                          \| / \\  |\n\
                          \|(_,_) |\n\
                          \|  I  " ++ [(snd h)] ++ "|\n\
                          \`------'\n" ++ (printHand t)
    where suit = fst h
