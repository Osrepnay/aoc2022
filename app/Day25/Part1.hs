module Day25.Part1 (thisMain) where

import           Data.Bifunctor  (first)
import qualified Data.List       as L
import           Data.List.Split
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day25" ReadMode
    contents <- hGetContents handle
    let nums = splitOn "\n" contents
    putStrLn $ reverse $ L.foldl' addLsdSnafu [] (reverse <$> nums)
    putStrLn $ reverse $ addLsdSnafu (reverse "1") (reverse "2")
    hClose handle

addLsdSnafu :: String -> String -> String
addLsdSnafu snafuA snafuB =
    let (snafu, carry) = L.foldl'
            (\(s, c) (a, b) -> first (: s) (addDigits a b c))
            ([], 0)
            (zip (rightpad maxLen snafuA) (rightpad maxLen snafuB))
    in reverse $ ["=", "-", "", "1", "2"] !! (carry + 2) ++ snafu
  where
    maxLen = max (length snafuA) (length snafuB)
    rightpad len s = s ++ replicate (len - length s) '0'
    digitValue :: Char -> Int
    digitValue x = case x of
        '2' -> 2
        '1' -> 1
        '0' -> 0
        '-' -> -1
        '=' -> -2
        _   -> error "no dogit"
        -- x -> error $ "no digit " ++ [x]
    addDigits a b c = case digitValue a + digitValue b + c of
        6  -> ('1', 1)
        5  -> ('0', 1)
        4  -> ('-', 1)
        3  -> ('=', 1)
        2  -> ('2', 0)
        1  -> ('1', 0)
        0  -> ('0', 0)
        -1 -> ('-', 0)
        -2 -> ('=', 0)
        -3 -> ('2', -1)
        -4 -> ('1', -1)
        -5 -> ('0', -1)
        -6 -> ('-', -1)
        _  -> error "ayo its screwed up g"
