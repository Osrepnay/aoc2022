module Day3.Part2 (thisMain) where

import           Data.Char
import qualified Data.List       as L
import           Data.List.Split
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day3" ReadMode
    contents <- hGetContents handle
    print $ sum $ itemPrio . head . commonAll <$> chunksOf 3 (lines contents)
    hClose handle

itemPrio :: Char -> Int
itemPrio c
    | ord c >= ord 'a' = ord c - ord 'a' + 1
    | otherwise        = ord c - ord 'A' + 27

commonAll :: Eq a => [[a]] -> [a]
commonAll (xs : xss) = L.foldl' common xs xss

common :: Eq a => [a] -> [a] -> [a]
common xs ys = filter (`elem` ys) xs
