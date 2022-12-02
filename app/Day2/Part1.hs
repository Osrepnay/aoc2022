module Day2.Part1 (thisMain) where

import Data.Char
import System.IO

data GameRes = Won | Lost | Draw

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day2" ReadMode
    contents <- hGetContents handle
    let matchups = (\x -> (head x, x !! 2)) <$> lines contents
    print $ matchupsScoreSum matchups
    hClose handle

matchupsScoreSum :: [(Char, Char)] -> Int
matchupsScoreSum ms = sum $ uncurry matchupScore <$> ms
  where
    matchupScore m o = ord o - ord 'X' + 1 +
        case result m o of
            Lost -> 0
            Draw -> 3
            Won  -> 6

result :: Char -> Char -> GameRes
result 'A' 'X' = Draw
result 'A' 'Y' = Won
result 'A' 'Z' = Lost
result 'B' 'X' = Lost
result 'B' 'Y' = Draw
result 'B' 'Z' = Won
result 'C' 'X' = Won
result 'C' 'Y' = Lost
result 'C' 'Z' = Draw
