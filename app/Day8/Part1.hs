module Day8.Part1 (thisMain) where

import qualified Data.List as L
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day8" ReadMode
    contents <- hGetContents handle
    let trees = lines contents
    print $ numVisible trees
    hClose handle

numVisible :: [[Char]] -> Int
numVisible trees = length
    [ (x, y)
    | x <- [0..length (head trees) - 1]
    , y <- [0..length trees - 1]
    , squareVisibleHorz x y trees || squareVisibleHorz y x ttrees
    ]
  where
    ttrees = L.transpose trees
    squareVisibleHorz x y ts =
        all (< currTree) firstHalf ||
        all (< currTree) secondHalf
      where
        currTree = row !! x
        (firstHalf, secondHalfIncl) = splitAt x row
        secondHalf = tail secondHalfIncl
        row = ts !! y
