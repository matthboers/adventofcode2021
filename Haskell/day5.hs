module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List ( (\\) )
import qualified Data.Text as T 
import qualified Data.Map as M
import Data.Bifunctor ( Bifunctor(bimap) )

main :: IO ()
main = do
  handle <- openFile "Input/day5.txt" ReadMode
  contents <- hGetContents handle

  let listLines = map (splitArrow . T.pack) (lines contents)
  let coordinatePairs = map (bimap splitParseComma splitParseComma) listLines

  let nonDiagonals = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) coordinatePairs
  let onlyDiagonals = coordinatePairs \\ nonDiagonals

  let foundPoints = foldr (\tup list -> getLinePointsStraight tup ++ list) [] nonDiagonals 
                 ++ foldr (\tup list -> getLinePointsDiagonal tup ++ list) [] onlyDiagonals
  
  print $ length $ crossedLines foundPoints

crossedLines :: (Ord a) => [a] -> [(a, Int)]
crossedLines xs = M.toList $ M.filter (>1) (M.fromListWith (+) [(x, 1) | x <- xs])

getLinePointsStraight :: ((Integer, Integer), (Integer, Integer)) -> [(Int, Int)]
getLinePointsStraight ((x1, y1), (x2, y2)) = [(fromIntegral x, fromIntegral y) | x <- xs, y <- ys]
  where xs = if x1 <= x2 then [x1..x2] else [x1, pred x1..x2]
        ys = if y1 <= y2 then [y1..y2] else [y1, pred y1..y2]

getLinePointsDiagonal :: ((Integer, Integer), (Integer, Integer)) -> [(Int, Int)]
getLinePointsDiagonal ((x1, y1), (x2, y2)) = [(fromIntegral x, fromIntegral y) | (x, y) <- zip xs ys]
  where xs = if x1 <= x2 then [x1..x2] else [x1, pred x1..x2]
        ys = if y1 <= y2 then [y1..y2] else [y1, pred y1..y2]

-- VERY HANDIG --
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)


-- PARSING --
breakOnExclusive :: T.Text -> T.Text -> (T.Text, T.Text)
breakOnExclusive splitter input = (T.strip lhs, T.strip rhs) 
  where (lhs, rhs_) = T.breakOn splitter input
        rhs = T.drop (length $ T.unpack splitter) rhs_

splitArrow :: T.Text -> (T.Text, T.Text)
splitArrow = breakOnExclusive (T.pack "->")

splitParseComma :: T.Text -> (Integer, Integer)
splitParseComma text = (read $ T.unpack s1 :: Integer, read $ T.unpack s2 :: Integer) 
  where (s1, s2) = breakOnExclusive (T.pack ",") text