module Day3.Part1 (thisMain) where

import           Data.Char
import qualified Data.List as L
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day3" ReadMode
    contents <- hGetContents handle
    let commons = head . L.nub . uncurry L.intersect . halves <$> lines contents
    print $ sum $ itemPrio <$> commons
    hClose handle

itemPrio :: Char -> Int
itemPrio c
    | ord c >= ord 'a' = ord c - ord 'a' + 1
    | otherwise        = ord c - ord 'A' + 27

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs
