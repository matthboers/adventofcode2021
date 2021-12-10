{-# LANGUAGE MultiWayIf #-}

module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Text.Printf ( printf )
import Data.List
import Data.Either

main :: IO ()
main = do
    contents <- readFile "Input/day10.txt"
    let puzzles = lines contents

    printf "Part 1 solution: %s\n" $ show $ sum $ map (firstIllegal . bracketPairs) puzzles

    let noIllegalPuzzles = filter (\x -> firstIllegal (bracketPairs x) == 0) puzzles
    let allScores = sort $ map (judgeAutoComplete . findUnmatchedBrackets) noIllegalPuzzles
    printf "Part 2 solution: %s\n" $ show $ allScores !! (length allScores `div` 2)

-- PART 1 --
firstIllegal :: Either String Char -> Int
firstIllegal (Left str) = if any (`elem` str) closingBrackets then p1Score (head str) else 0
firstIllegal (Right c)  = p1Score c

p1Score :: Char -> Int
p1Score ')' = 3
p1Score ']' = 57
p1Score '}' = 1197
p1Score '>' = 25137
p1Score _ = 0


bracketPairs :: String -> Either String Char
bracketPairs (x:xs) | x `elem` closingBrackets = Left (x:xs)
                    | otherwise = if 
                        | Left (y:ys) <- next -> if x == openingBracket y then bracketPairs ys else Right y
                        | Left []     <- next -> Right ' '
                        | Right i     <- next -> Right i
                    where next = bracketPairs xs
bracketPairs [] = Right ' '

-- PART 2 --
findUnmatchedBrackets :: String ->  String
findUnmatchedBrackets str = recurStep str ""
  where recurStep (x:xs) ys = recurStep xs $ if x `elem` closingBrackets && openingBracket x `elem` ys
                                             then reverse $ openingBracket x `delete` reverse ys
                                             else ys ++ [x]
        recurStep [] ys = ys

judgeAutoComplete :: String -> Int
judgeAutoComplete (c:xs) = p2Score c + 5 * judgeAutoComplete xs
judgeAutoComplete [] = 0

p2Score :: Char -> Int
p2Score '(' = 1
p2Score '[' = 2
p2Score '{' = 3
p2Score '<' = 4
p2Score _ = 0


-- BOTH PARTS -- 
openingBrackets :: [Char]
closingBrackets :: [Char]
openingBrackets = ['(', '[', '{', '<']
closingBrackets = [')', ']', '}', '>']

openingBracket :: Char -> Char
openingBracket ')' = '('
openingBracket ']' = '['
openingBracket '}' = '{'
openingBracket '>' = '<'
openingBracket _ = ' '

closingBracket :: Char -> Char
closingBracket '(' = ')'
closingBracket '[' = ']'
closingBracket '{' = '}'
closingBracket '<' = '>'
closingBracket _ = ' '