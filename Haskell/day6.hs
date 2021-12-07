module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import qualified Data.Map as M

main :: IO ()
main = do
  handle <- openFile "Input/day6.txt" ReadMode
  contents <- hGetContents handle
  let fishTimers = map read $ commas contents
  let simulatedLanterns = steppedFishCounter 80 fishTimers
  --print $ length simulatedLanterns -- PART 1
  let emptyFishList = M.fromList [(x, 0) | x <- [0..8]]
  let fishCycleCount = map snd $ M.toList $ M.union (M.fromListWith (+) [(x, 1) | x <- fishTimers]) emptyFishList

  print $ sum $ runFishListStepper 256 fishCycleCount --PART 2



-- FANCIER AND FASTER SOLUTION --
runFishListStepper :: Integer -> [Integer] -> [Integer]
runFishListStepper 0 list = list 
runFishListStepper x list = runFishListStepper (x-1) $ fishListStepper list

fishListStepper :: [Integer] -> [Integer]
fishListStepper [] = []
fishListStepper (x:xs) = lhs ++ [x + y] ++ rhs ++ [x]
  where (lhs, y:rhs) = splitAt 6 xs


--          NAIVE IMPLEMENTATION            --
-- WORKS LOVELY FOR SMALLER NUMBER OF STEPS --
steppedFishCounter :: Integer -> [Integer] -> [Integer]
steppedFishCounter 0 fish = fish
steppedFishCounter x fish = steppedFishCounter (x - 1) $
                            foldr (\fish list -> runFishCounter fish ++ list) [] fish

runFishCounter :: Integer -> [Integer]
runFishCounter 0 = [6, 8]
runFishCounter x = [x - 1]

-- VERY HANDIG --
commas :: String -> [String]
commas [] = []
commas s = x : ys
  where (x, xs) = break (==',') s
        ys = commas $ drop 1 xs 