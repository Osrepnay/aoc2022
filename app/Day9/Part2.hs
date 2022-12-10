module Day9.Part2 (thisMain) where

import           Data.Bifunctor
import qualified Data.List      as L
import           Data.Set
import qualified Data.Set       as S
import           System.IO

type Point = (Int, Int)
data SnakeSpace = SnakeSpace [(Int, Int)] (Set Point) deriving Show

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day9" ReadMode
    contents <- hGetContents handle
    let moves = lines contents
    let (SnakeSpace _ ps) = allMoves moves (SnakeSpace (replicate 10 (0, 0)) S.empty)
    print $ S.size ps
    hClose handle

allMoves :: [String] -> SnakeSpace -> SnakeSpace
allMoves (move : moves) ss = allMoves moves (iterate moveWithF ss !! times)
  where
    moveWords = words move
    direction = head moveWords
    times = read $ moveWords !! 1
    moveWithF = moveSnake $ case direction of
        "U" -> second (+ 1)
        "D" -> second (subtract 1)
        "R" -> first (+ 1)
        "L" -> first (subtract 1)
        _   -> error "unknown direction"
allMoves [] ss = ss

moveSnake :: ((Int, Int) -> (Int, Int)) -> SnakeSpace -> SnakeSpace
moveSnake f (SnakeSpace snake@(h : _) visited) = SnakeSpace
    (newHead : newSnakeTail)
    (S.insert (last newSnakeTail) visited)
  where
    newSnakeTail = moveParts newHead snake
    newHead = f h
    moveParts lastHT (_ : body@(headTail : _)) =
        newHeadTail : moveParts newHeadTail body
      where
        newHeadTail = movePair lastHT headTail
    moveParts _ [_] = []
    moveParts _ [] = []

chebyshev :: (Int, Int) -> (Int, Int) -> Int
chebyshev (ax, ay) (bx, by) = max (abs (ax - bx)) (abs (ay - by))

movePair :: (Int, Int) -> (Int, Int) -> (Int, Int)
movePair pairHead@(hx, hy) pairTail@(tx, ty)
    | chebyshev pairHead pairTail > 1 = newTail
    | otherwise                       = pairTail
  where
    newTail = (tx + signum (hx - tx), ty + signum (hy - ty))
