module Day12.Part1 (thisMain) where

import           Data.Char
import qualified Data.List     as L
import           Data.Maybe
import           Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as S
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day12" ReadMode
    contents <- hGetContents handle
    let myIdx = fromJust $ L.elemIndex 'S' contents
    let heightmap = lines contents
    let myX = myIdx `rem` (length (head heightmap) + 1)
    let myY = myIdx `quot` (length (head heightmap) + 1)
    print $ minpath (myX, myY) (length (head heightmap), length heightmap) heightmap
    hClose handle

minpath :: (Int, Int) -> (Int, Int) -> [[Char]] -> Int
minpath orig (width, height) heightmap = minpath'
    (S.singleton orig)
    (Seq.singleton (orig, 0))
  where
    elevation 'E' = ord 'z'
    elevation 'S' = ord 'a'
    elevation x   = ord x
    dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    -- stop copying heightmap
    minpath' visited (((x, y), d) :<| frontier)
        | heightmap !! y !! x == 'E' = d
        | otherwise = minpath' afterVisited
            $ frontier
            >< Seq.fromList ((\p -> (p, d + 1)) <$> posses)
      where
        afterVisited = L.foldl' (\b a -> S.insert a b) visited posses
        posses =
            [ (nx, ny)
            | (dx, dy) <- dirs
            , let nx = x + dx, let ny = y + dy
            , 0 <= nx && nx < width
            , 0 <= ny && ny < height
            , not (S.member (nx, ny) visited)
            , elevation (heightmap !! ny !! nx)
                - elevation (heightmap !! y !! x)
                <= 1
            ]
    minpath' _ _ = error "empty frontier"
