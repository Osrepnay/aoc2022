module Day14.Part2 (thisMain) where

import           Data.Bifunctor
import           Data.Char
import qualified Data.List       as L
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
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
    let dropAllTheSand = iterate (dropSandAt (500, 0) walls) (S.empty, M.empty)
    print $ fromJust $ L.findIndex (S.member (500, 0) . fst) $ dropAllTheSand
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

onFloor :: (Int, Int) -> Walls -> Bool
onFloor (_, y) (Walls _ hy) = y == (hy + 1)

dropSandAt
    :: (Int, Int)
    -> Walls
    -> (Set (Int, Int), Map (Int, Int) Bool)
    -> (Set (Int, Int), Map (Int, Int) Bool)
dropSandAt point walls (sands, occupancy) = dropSand point
  where
    dropSand (x, y)
        | onFloor (x, y) walls = (inserted, occupancyInserted)
        | unCenter             = second oC $ dropSand (x, y + 1)
        | unLeft               = second (oC . oL) $ dropSand (x - 1, y + 1)
        | unRight              = second (oC . oL . oR) $ dropSand (x + 1, y + 1)
        | otherwise            = (tsands, toccupancy)
      where
        (unCenter, oC) = unoccupied (x, y + 1)
        (unLeft, oL) = unoccupied (x - 1, y + 1)
        (unRight, oR) = unoccupied (x + 1, y + 1)
        unoccupied p = case (M.lookup p occupancy) of
           Just r  -> (r, id)
           Nothing ->
               let r = not $ S.member p sands || pointInWall p walls
                in
                    ( r
                    , \o ->
                        if fromMaybe True (M.lookup p o)
                           then M.insert p r o
                           else o)
        tsands = S.delete (x, y + 2) inserted
        toccupancy = M.delete (x, y + 2) occupancyInserted
        inserted = S.insert (x, y) sands
        occupancyInserted = M.insert (x, y) False occupancy
