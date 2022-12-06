module Day6.Part2 (thisMain) where

import qualified Data.List       as L
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day6" ReadMode
    contents <- hGetContents handle
    print $ scanStartPacket contents + 14
    hClose handle

scanStartPacket :: String -> Int
scanStartPacket = scanCount 0
  where
    scanCount c s
        | fourDiff (take 14 s) = c
        | otherwise           = scanCount (c + 1) (tail s)
    fourDiff s = length s == length (L.nub s)
