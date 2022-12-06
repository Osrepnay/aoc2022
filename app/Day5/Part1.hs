module Day5.Part1 (thisMain) where

import           Data.Char
import qualified Data.List       as L
import           Data.List.Split
import           System.IO

data Move = Move Int Int Int deriving (Show)

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day5" ReadMode
    contents <- hGetContents handle
    let (crates : moves : []) = splitOn "\n\n" contents
    let parsedCrates = parseCrates crates
    let parsedMoves = parseMove <$> lines moves
    putStrLn $ head <$> L.foldl' doMove parsedCrates parsedMoves
    hClose handle

doMove :: [[Char]] -> Move -> [[Char]]
doMove crates (Move num from to) = modIdx (took ++) (to - 1) $
    modIdx (drop num) (from - 1) crates
  where
    took = reverse $ take num $ crates !! (from - 1)
    modIdx f i xs = front ++ (f x : back)
      where
        (front, (x : back)) = splitAt i xs

parseCrates :: String -> [[Char]]
parseCrates crates = filter (not . null) $
    fmap (filter isAlphaNum) $
    L.transpose $
    lines crates

parseMove :: String -> Move
parseMove s = Move (read $ sWords !! 1) (read $ sWords !! 3) (read $ sWords !! 5)
  where
    sWords = words s
