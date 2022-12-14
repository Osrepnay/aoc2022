module Day14.Part1 (thisMain) where

import           Data.Char
import qualified Data.List       as L
import           Data.List.Split
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as S
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day14" ReadMode
    contents <- hGetContents handle
    let wallsRaw = fmap (listToPoint . fmap read . splitOn ",")
            . filter (isDigit . head)
            . words
            <$> lines contents
    let walls = Walls (wallsRaw >>= pairs) (maximum $ wallsRaw >>= fmap snd)
    let dropAllTheSand = iterate (dropSandAt (500, 0) walls) S.empty
    print $ fromJust $ L.findIndex (\(a, b) -> a == b) $ pairs $ dropAllTheSand
    hClose handle

data Walls = Walls [((Int, Int), (Int, Int))] Int deriving (Show)

listToPoint :: [a] -> (a, a)
listToPoint [x, y] = (x, y)
listToPoint _      = error "wrong size list"

pairs :: [a] -> [(a, a)]
pairs []               = []
pairs [_]              = []
pairs (a : xs@(b : _)) = (a, b) : pairs xs

between :: Ord a => a -> a -> a -> Bool
between a x b
    | a > b     = between b x a
    | otherwise = a <= x && x <= b

intersects :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
intersects (x, y) ((iax, iay), (ibx, iby))
    | iax == ibx = x == iax && between iay y iby
    | iay == iby = y == iay && between iax x ibx
    | otherwise  = error "lines are diagonal"

pointInWall :: (Int, Int) -> Walls -> Bool
pointInWall point (Walls walls _) = any (intersects point) walls

underAll :: (Int, Int) -> Walls -> Bool
underAll (_, y) (Walls _ hy) = y > hy

dropSandAt :: (Int, Int) -> Walls -> Set (Int, Int) -> Set (Int, Int)
dropSandAt (x, y) walls sands
    | underAll (x, y) walls     = sands
    | unoccupied (x, y + 1)     = dropSandAt (x, y + 1) walls sands
    | unoccupied (x - 1, y + 1) = dropSandAt (x - 1, y + 1) walls sands
    | unoccupied (x + 1, y + 1) = dropSandAt (x + 1, y + 1) walls sands
    | otherwise                 = S.insert (x, y) sands
  where
    unoccupied p = not $ S.member p sands || pointInWall p walls
