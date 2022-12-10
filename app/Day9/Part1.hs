module Day9.Part1 (thisMain) where

import           Data.Bifunctor
import           Data.Set
import qualified Data.Set       as S
import           System.IO

type Point = (Int, Int)
data SnakeSpace = SnakeSpace (Int, Int) (Int, Int) (Set Point) deriving Show

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day9" ReadMode
    contents <- hGetContents handle
    let moves = lines contents
    let (SnakeSpace _ _ ps) = allMoves moves (SnakeSpace (0, 0) (0, 0) S.empty)
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

chebyshev :: (Int, Int) -> (Int, Int) -> Int
chebyshev (ax, ay) (bx, by) = max (abs (ax - bx)) (abs (ay - by))

moveSnake :: ((Int, Int) -> (Int, Int)) -> SnakeSpace -> SnakeSpace
moveSnake f (SnakeSpace snakeHead snakeTail tailVisited) = SnakeSpace
    newHead
    newTail
    (S.insert newTail tailVisited)
  where
    newHead = f snakeHead
    newTail
        | chebyshev newHead snakeTail > 1 = snakeHead
        | otherwise                       = snakeTail
