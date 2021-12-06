module Main where

import System.IO
import Text.Printf

main :: IO ()
main = do
  handle <- openFile "/mnt/c/Users/matth/Desktop/Advent Of Code 2021/Input/day1.txt" ReadMode
  contents <- hGetContents handle
  let inWords = words contents
  let numberList = map (\x -> read x :: Integer) inWords

  printf "Part 1 solution: %s\n" (part1 numberList)
  printf "Part 2 solution: %s\n" (part2 numberList)
  hClose handle

part1 :: [Integer] -> String 
part1 numList = show $ foldl (\sum (old, new) -> if new > old then sum + 1 else sum) 0 comparingPairs
    where comparingPairs = zip numList (tail numList)

part2 :: [Integer] -> String 
part2 numList = part1 summedTriples
  where summedTriples = zipWith3 (\a b c -> a + b + c) numList (tail numList ++ [0]) (tail $ tail numList ++ [0,0])