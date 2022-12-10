module Day8.Part2 (thisMain) where

import qualified Data.List  as L
import           Data.Maybe
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day8" ReadMode
    contents <- hGetContents handle
    let trees = lines contents
    print $ scenicScores trees
    hClose handle

scenicScores :: [[Char]] -> Int
scenicScores trees = L.maximum
    [ scenicity
    | x <- [1..length (head trees) - 2]
    , y <- [1..length trees - 2]
    , let scenicity = scenicityHorz x y trees * scenicityHorz y x ttrees
    ]
  where
    ttrees = L.transpose trees
    scenicityHorz x y ts =
        scenicity (reverse firstHalf) *
        scenicity secondHalf
      where
        scenicity r = fromMaybe (length r) ((+ 1) <$> L.findIndex (>= currTree) r)
        currTree = row !! x
        (firstHalf, secondHalfIncl) = splitAt x row
        secondHalf = tail secondHalfIncl
        row = ts !! y
