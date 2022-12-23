module Day23.Part1 (thisMain) where

import           Data.List ((\\))
import qualified Data.List as L
import           Data.Map  (Map, (!))
import qualified Data.Map  as M
import qualified Data.Set  as S
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day23" ReadMode
    contents <- hGetContents handle
    let elves = parseElves $ lines contents
    let elvesEnd = M.keys $ doRounds 10 elves
    let (endXs, endYs) = unzip elvesEnd
    let rectWidth = maximum endXs - minimum endXs + 1
    let rectHeight = maximum endYs - minimum endYs + 1
    print $ rectWidth * rectHeight - length elvesEnd
    hClose handle

doRounds :: Int -> Map (Int, Int) Int -> Map (Int, Int) Int
doRounds 0 elves = elves
doRounds rounds elves = doRounds (rounds - 1) (tryMove proposal elves)
  where
    proposal = propose elves

tryMove
    :: [((Int, Int), (Int, Int))]
    -> Map (Int, Int) Int
    -> Map (Int, Int) Int
tryMove moves elves = M.fromList
    $ (\(k, a) -> (a, elves ! k + 1)) <$> validMoves
  where
    validMoves = (\(s, m) -> if S.member m conflicting then (s, s) else (s, m))
        <$> moves
    allTargets = snd <$> moves
    conflicting = S.fromList $ allTargets \\ L.nub allTargets

propose :: Map (Int, Int) Int -> [((Int, Int), (Int, Int))]
propose elves = (\(k, _) -> (k, elfPropose k)) <$> M.toList elves
  where
    elfPropose (x, y)
        | allDirs           = (x, y)
        | any fst cardinals = snd $ head $ dropWhile (not . fst) cardinals
        | otherwise         = (x, y)
      where
        allDirs = and aroundOpen
        cardinals = take 4 $ drop (elves ! (x, y))
            $ cycle
                [ (north, (x, y - 1))
                , (south, (x, y + 1))
                , (west, (x - 1, y))
                , (east, (x + 1, y))
                ]
        north = aroundOpen !! 0 && aroundOpen !! 1 && aroundOpen !! 2
        south = aroundOpen !! 5 && aroundOpen !! 6 && aroundOpen !! 7
        west = aroundOpen !! 0 && aroundOpen !! 3 && aroundOpen !! 5
        east = aroundOpen !! 2 && aroundOpen !! 4 && aroundOpen !! 7
        aroundOpen = not . flip M.member elves <$>
            [ (x - 1, y - 1)
            , (x, y - 1)
            , (x + 1, y - 1)
            , (x - 1, y)
            , (x + 1, y)
            , (x - 1, y + 1)
            , (x, y + 1)
            , (x + 1, y + 1)
            ]

parseElves :: [[Char]] -> Map (Int, Int) Int
parseElves elfGrid = M.fromList $ (\x -> (x, 0)) <$> elfSquares
  where
    elfSquares =
        [ (x, y)
        | y <- [0..length elfGrid - 1]
        , x <- [0..length (head elfGrid) - 1]
        , elfGrid !! y !! x == '#'
        ]
