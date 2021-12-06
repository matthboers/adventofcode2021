{-# LANGUAGE TupleSections #-}

module Main where

import System.IO
import System.IO.Error
import Text.Printf
import Data.List
import Data.Maybe

main :: IO ()
main = do
  handle <- openFile "/mnt/c/Users/matth/Desktop/Advent Of Code 2021/Input/day4.txt" ReadMode
  numbersStr <- hGetLine handle
  bingoCardsStr <- retrieveAllBingo handle []
  let bingoCards = map (map (map (\x -> read x :: Integer) . words)) bingoCardsStr
  let numbers = map read $ words numbersStr
  
  let (bingo, pulled) = keepLastBoard numbers bingoCards
  let leftOver = sum $ map (\x -> sum $ x \\ pulled) bingo

  print $ leftOver * head pulled

  hClose handle


keepLastBoard :: [Integer] -> [[[Integer]]] -> ([[Integer]], [Integer])
keepLastBoard nums [board] = iterateOverNumbers ([], nums) [board]
keepLastBoard nums boards  = keepLastBoard nums $ delete (fst $ iterateOverNumbers ([], nums) boards) boards


iterateOverNumbers :: ([Integer], [Integer]) -> [[[Integer]]] -> ([[Integer]], [Integer])
iterateOverNumbers (pulled, [])   boards = ([[]], []) 
iterateOverNumbers (pulled, x:xs) boards = either (, pulled) id resultBoard
    where resultBoard = maybeToLeft (iterateOverNumbers (x : pulled, xs) boards) (find (confirmWinningBoard pulled) boards)



confirmWinningBoard :: [Integer] -> [[Integer]] -> Bool
confirmWinningBoard pulled board = any ((== True) . lineCheck) board || any ((== True) . lineCheck) (transpose board)
            where lineCheck line = all ((== True) . (`elem` pulled)) line





retrieveAllBingo :: Handle -> [[String]] -> IO [[String]]
retrieveAllBingo handle xs = do
                            _ <- tryIOError $ hGetLine handle -- Discard one empty line
                            ys <- ysIO 
                            if null ys then return xs else
                              retrieveAllBingo handle $ xs ++ [ys]
    where ysIO = catchIOError (retrieveBingo handle) (\_ -> return [])

retrieveBingo :: Handle -> IO [String]
retrieveBingo handle = retStep 5
   where retStep :: Integer -> IO [String]
         retStep 0 = return []
         retStep n = do 
             content <- hGetLine handle 
             nextContent <- retStep (n - 1)
             return $ content : nextContent

maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft _ (Just x) = Left x
maybeToLeft y Nothing  = Right y