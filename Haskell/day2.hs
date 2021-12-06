module Main where

import System.IO
import Text.Printf

main :: IO ()
main = do
  handle <- openFile "/mnt/c/Users/matth/Desktop/Advent Of Code 2021/Input/day2.txt" ReadMode
  contents <- hGetContents handle
  let inWords = words contents
  let directionPairs = map parseInput inWords
  printf "Part 1 solution: %s\n" (part1 directionPairs)
  printf "Part 2 solution: %s\n" (part2 directionPairs)
  hClose handle

parseInput :: String -> (Integer, Integer)
parseInput str
  | dir == "forward" = (scalar, 0)
  | dir == "down"    = (0, scalar)
  | dir == "up"      = (0, -scalar)
  | otherwise = (0, 0)
  where
    (dir, scalarStr) = break (== ' ') str
    scalar = read $ drop 1 scalarStr


part1 :: [(Integer, Integer)] -> String
part1 nums = show $ horiz * depth
  where
    (horiz, depth) = foldl (\(a, b) (u, v) -> (a + u, b + v)) (0, 0) nums

part2 :: [(Integer, Integer)] -> String
part2 nums = show $ horiz * depth
  where
    (nums1, nums2) = unzip nums
    tripleNums = zip3 nums1 nums2 (repeat 0)
    (aim, horiz, depth) = foldl (\(a, b, c) (u, v, w) -> (a + u, b + v, c + a * v)) (0, 0, 0) tripleNums