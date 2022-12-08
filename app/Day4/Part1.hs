module Day4.Part1 (thisMain) where

import           Data.List.Split
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day4" ReadMode
    contents <- hGetContents handle
    let sectionPairs = listTup2 . fmap (listTup2 . fmap read . splitOn "-") . splitOn "," <$> lines contents
    print $ length $ filter (uncurry firstContains) sectionPairs
    hClose handle

listTup2 :: [a] -> (a, a)
listTup2 xs = (head xs, xs !! 1)

firstContains :: (Int, Int) -> (Int, Int) -> Bool
firstContains ec@(a, b) e@(c, d)
    | a == c    = True
    | a > c     = firstContains e ec
    | otherwise = b >= d
