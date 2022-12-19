module Day18.Part1 (thisMain) where

import           Data.List.Split
import           Data.Set        (Set)
import qualified Data.Set        as S
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day18" ReadMode
    contents <- hGetContents handle
    let cubes = S.fromList $ parseCube <$> lines contents
    print $ sum $ visibleSides cubes <$> S.toList cubes
    hClose handle

data Point3d = Point3d Int Int Int deriving (Eq, Ord, Show)

visibleSides :: Set Point3d -> Point3d -> Int
visibleSides ps (Point3d x y z) = sum
    $ boolToInt
    . not
    . flip S.member ps
    <$>
        [ Point3d (x - 1) y z
        , Point3d (x + 1) y z
        , Point3d x (y - 1) z
        , Point3d x (y + 1) z
        , Point3d x y (z - 1)
        , Point3d x y (z + 1)
        ]
  where
    boolToInt False = 0
    boolToInt True  = 1

parseCube :: String -> Point3d
parseCube s = Point3d (head coords) (coords !! 1) (coords !! 2)
  where
    coords = read <$> splitOn "," s
