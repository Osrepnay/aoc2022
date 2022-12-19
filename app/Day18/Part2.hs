module Day18.Part2 (thisMain) where

import qualified Data.List       as L
import           Data.List.Split
import           Data.Set        (Set)
import qualified Data.Set        as S
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day18" ReadMode
    contents <- hGetContents handle
    let cubesList = parseCube <$> lines contents
    let cubes = S.fromList cubesList
    let maxCoords = Point3d
            (maximum (pointX <$> cubesList) + 1)
            (maximum (pointY <$> cubesList) + 1)
            (maximum (pointZ <$> cubesList) + 1)
    let minCoords = Point3d
            (minimum (pointX <$> cubesList) - 1)
            (minimum (pointY <$> cubesList) - 1)
            (minimum (pointZ <$> cubesList) - 1)
    let filled = fill minCoords S.empty cubes (minCoords, maxCoords)
    let sides = concatMap borders (S.toList filled) `L.intersect` cubesList
    print $ length sides
    hClose handle

data Point3d = Point3d
    { pointX :: Int
    , pointY :: Int
    , pointZ :: Int
    } deriving (Eq, Ord, Show)

pointInRange :: Point3d -> (Point3d, Point3d) -> Bool
pointInRange (Point3d x y z) (Point3d lx ly lz, Point3d hx hy hz) =
    lx <= x && x <= hx && ly <= y && y <= hy && lz <= z && z <= hz

borders :: Point3d -> [Point3d]
borders (Point3d x y z) =
    [ Point3d (x - 1) y z
    , Point3d (x + 1) y z
    , Point3d x (y - 1) z
    , Point3d x (y + 1) z
    , Point3d x y (z - 1)
    , Point3d x y (z + 1)
    ]

fill
    :: Point3d
    -> Set Point3d
    -> Set Point3d
    -> (Point3d, Point3d)
    -> Set Point3d
fill p visited avoid range
    | S.member p avoid = S.empty
    | S.member p visited = S.empty
    | not (pointInRange p range) = S.empty
    | otherwise = L.foldl'
        (\b a -> S.union b (fill a b avoid range))
        newVisited
        (borders p)
  where
    newVisited = S.insert p visited

parseCube :: String -> Point3d
parseCube s = Point3d (head coords) (coords !! 1) (coords !! 2)
  where
    coords = read <$> splitOn "," s
