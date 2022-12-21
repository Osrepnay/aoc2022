module Day20.Part2 (thisMain) where

import qualified Data.List as L
import Data.Maybe
import System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day20" ReadMode
    contents <- hGetContents handle
    let numbers = zip ((* 811589153) . read <$> lines contents) [0..]
    let mixLen = length numbers
    let mixed = dropWhile ((/= 0) . fst)
            $ cycle
            $ (!! 10)
            $ iterate (mix numbers mixLen) numbers
    print $ sum $ fst . (mixed !!) <$> [1000, 2000, 3000]
    hClose handle

mix :: [(Int, Int)] -> Int -> [(Int, Int)] -> [(Int, Int)]
mix [] _ toMix = toMix
mix (n : ns) mixLen toMix =  mix
    ns
   mixLen
   (insertAt moveTo n (deleteAt moveFrom toMix))
  where
    moveFrom = fromJust $ L.elemIndex n toMix
    moveTo = (moveFrom + fst n) `mod` (mixLen - 1)

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 n xs = n : xs
insertAt _ _ [] = error "insert index too big"
insertAt ci n (x : xs) = x : insertAt (ci - 1) n xs

deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = error "delete index too big"
deleteAt 0 (_ : xs) = xs
deleteAt ci (x : xs) = x : deleteAt (ci - 1) xs
