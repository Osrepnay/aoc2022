module Day2.Part2 (thisMain) where

import Data.Char
import System.IO

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
    matchupScore m o = ord (result m o) - ord 'A' + 1 +
        case o of
            'X' -> 0
            'Y' -> 3
            'Z' -> 6

result :: Char -> Char -> Char
result 'A' 'X' = 'C'
result 'A' 'Y' = 'A'
result 'A' 'Z' = 'B'
result 'B' 'X' = 'A'
result 'B' 'Y' = 'B'
result 'B' 'Z' = 'C'
result 'C' 'X' = 'B'
result 'C' 'Y' = 'C'
result 'C' 'Z' = 'A'
