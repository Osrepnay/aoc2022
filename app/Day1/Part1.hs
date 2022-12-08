module Day1.Part1 (thisMain) where

import           Data.List.Split
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day1" ReadMode
    contents <- hGetContents handle
    let elves = fmap read . lines <$> splitOn "\n\n" contents
    print $ biggestElfCalories elves
    hClose handle

biggestElfCalories :: [[Int]] -> Int
biggestElfCalories = maximum . fmap sum
