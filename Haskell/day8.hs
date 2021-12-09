{-# LANGUAGE MultiWayIf #-}

module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Text.Printf ( printf )
import Data.Bifunctor ( Bifunctor(first) )
import Data.List ( isSubsequenceOf, (\\), sort )
import qualified Data.Map as Map

type TranslationMap = Map.Map String Int

knownNums :: [Int]
knownNums = [2, 3, 4, 7]

main :: IO ()
main = do
  handle <- openFile "Input/day8.txt" ReadMode
  contents <- hGetContents handle

  let puzzles = map (splitExclusive "|" . words) $ lines contents
  printf "Part 1 solution: %s\n" $ show $ sum $ map (length . filter (\ x -> length x `elem` knownNums) . snd) puzzles
  printf "Part 2 solution: %s\n" $ show $ sum $ map solveStep puzzles

-- Solving Part 2
solveStep :: ([String], [String]) -> Integer 
solveStep (toTranslate, toDecipher) = let translationMap = findKnownNums toTranslate 
                                      in read (concatMap (show . findNumber translationMap) toDecipher)

-- Translate from String to Integer
findNumber :: TranslationMap -> String -> Int
findNumber inMap inNum | Just result <- easyNum = result
                       | length num == 6 = if 
                         | subset (l 4) num        -> 9
                         | subset (l 7) num        -> 0
                         | otherwise               -> 6
                       | length num == 5 = if 
                         | subset (l 7) num        -> 3
                         | subset (l 4 \\ l 1) num -> 5
                         | otherwise               -> 2
                       | otherwise = undefined
  where num = sort inNum
        easyNum = Map.lookup num inMap 
        invmap  = Map.fromList $ map (\(k, vs) -> (vs, k)) (Map.toList inMap)
        x `subset` y = x `isSubsequenceOf` y -- All elements of x are part of y
        l x = invmap Map.! x

-- For lengths of which we can be sure what the number will be
-- Enter String, this method takes it's length
-- If it is a length we can judge with certainty, Link the number to it in a map
-- Otherwise, discard.
knownNumsMapping :: String -> TranslationMap
knownNumsMapping s | iL == 2   = Map.singleton (sort s) 1 
                   | iL == 3   = Map.singleton (sort s) 7 
                   | iL == 4   = Map.singleton (sort s) 4 
                   | iL == 7   = Map.singleton (sort s) 8 
                   | otherwise = Map.empty 
               where iL = length s

-- Reduce every number gained from knownNumsMapping to it's minimum set
findKnownNums :: [String] -> TranslationMap
findKnownNums = foldr (Map.union . knownNumsMapping) Map.empty

-- VERY HANDIG --
splitExclusive :: (Eq a) => a -> [a] -> ([a], [a])
splitExclusive tgt list = (lhs, drop 1 rhs)
    where (lhs, rhs) = span (/= tgt) list 