module Day12.Part2 (thisMain) where

import           Data.Char
import qualified Data.List     as L
import           Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set      as S
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day12" ReadMode
    contents <- hGetContents handle
    let myIdxs = L.elemIndices 'a' contents
    let heightmap = lines contents
    let myXs = (`rem` (length (head heightmap) + 1)) <$> myIdxs
    let myYs = (`quot` (length (head heightmap) + 1)) <$> myIdxs
    let myPoss = zip myXs myYs
    print
        $ minimum
        $ flip fmap myPoss
        $ \x -> minpath x (length (head heightmap), length heightmap) heightmap
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
    minpath' _ _ = maxBound `quot` 2
