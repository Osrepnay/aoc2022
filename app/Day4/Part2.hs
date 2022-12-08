module Day4.Part2 (thisMain) where

import           Data.List.Split
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day4" ReadMode
    contents <- hGetContents handle
    let sectionPairs = listTup2 . fmap (listTup2 . fmap read . splitOn "-") . splitOn "," <$> lines contents
    print $ length $ filter (uncurry overlaps) sectionPairs
    hClose handle

listTup2 :: [a] -> (a, a)
listTup2 xs = (head xs, xs !! 1)

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps ec@(a, b) e@(c, _)
    | a == c    = True
    | a > c     = overlaps e ec
    | otherwise = b >= c
