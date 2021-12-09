module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Text.Printf ( printf )

main :: IO ()
main = do
  handle <- openFile "Input/day7.txt" ReadMode
  contents <- hGetContents handle

  let nums = map (\x -> read x :: Integer) $ commas contents
  let rangeNums = [minimum nums.. maximum nums]
  
  printf "Part 1 solution: %s\n" $ show $ minimum $ map (sumDistance nums) rangeNums
  printf "Part 2 solution: %s\n" $ show $ minimum $ map (sumTriangleDistance nums) rangeNums


sumDistance :: [Integer] -> Integer -> Integer
sumDistance distList tgt = foldr (\dist total -> abs (dist - tgt) + total) 0 distList

sumTriangleDistance :: [Integer] -> Integer -> Integer
sumTriangleDistance distList tgt = foldr (\dist total -> triangleNumber dist total + total) 0 distList
  where triangleNumber dist total = let n = abs (dist - tgt) in
                                    n * (n+1) `div` 2


-- VERY HANDIG --
commas :: String -> [String]
commas [] = []
commas s = x : ys
  where (x, xs) = break (==',') s
        ys = commas $ drop 1 xs 