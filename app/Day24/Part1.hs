module Day24.Part1 (thisMain) where

import           Data.Bifunctor
import           Data.IntMap    ((!), (!?))
import qualified Data.IntMap    as IM
import qualified Data.List      as L
import           Data.Maybe
import           Data.Set       (Set)
import qualified Data.Set       as S
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day24" ReadMode
    contents <- hGetContents handle
    let valleyGrid = lines contents
    let startPos = (fromJust $ L.elemIndex '.' $ head valleyGrid, 0)
    let endPos =
            ( fromJust $ L.elemIndex '.' $ last valleyGrid
            , length valleyGrid - 1
            )
    let valley = parseValley valleyGrid
    let valleyBounds = Bounds
            (1, length (head valleyGrid) - 2)
            (1, length valleyGrid - 2)
            startPos
            endPos
    print $ exitMins endPos valleyBounds valley startPos
    hClose handle

data Bounds = Bounds (Int, Int) (Int, Int) (Int, Int) (Int, Int)

inBounds :: Bounds -> (Int, Int) -> Bool
inBounds (Bounds (lx, hx) (ly, hy) start end) p@(x, y) = start == p
    || end == p
    ||
        (lx <= x
            && x <= hx
            && ly <= y
            && y <= hy)

wrapBounds :: Bounds -> (Int, Int) -> (Int, Int)
wrapBounds (Bounds (lx, hx) (ly, hy) _ _) (x, y)
    | lx > x    = (hx - (lx - x - 1), y)
    | hx < x    = (lx + (x - hx - 1), y)
    | ly > y    = (x, hy - (ly - y - 1))
    | hy < y    = (x, ly + (y - hy - 1))
    | otherwise = (x, y)

data Blizzards = Blizzards
    { ups    :: Set (Int, Int)
    , rights :: Set (Int, Int)
    , downs  :: Set (Int, Int)
    , lefts  :: Set (Int, Int)
    } deriving Show

noneIn :: Blizzards -> (Int, Int) -> Bool
noneIn (Blizzards u r d l) at = not
    $ S.member at u
    || S.member at r
    || S.member at d
    || S.member at l

exitMins :: (Int, Int) -> Bounds -> Blizzards -> (Int, Int) -> Int
exitMins exit bounds startBlizzards start = go 0
    $ IM.singleton 0
    $ S.singleton start
  where
    allBlizzies = iterate (moveBlizzards bounds) startBlizzards
    go d frontier
        | IM.null frontier   = 99999999
        | S.null lowestMoves = go (d + 1) (IM.delete d frontier)
        | exit == at         = d
        | otherwise          = go
            d
            updatedFrontier
      where
        at@(x, y) = head $ S.toList lowestMoves
        lowestMoves = frontier ! d
        thisBlizzies = allBlizzies !! (d + 1)
        updatedFrontier = IM.insert d currMoves
            $ IM.insert (d + 1) nextMoves frontier
          where
            currMoves = S.delete at lowestMoves
            nextMoves = S.union (S.fromList goodMoves)
                $ fromMaybe S.empty
                $ frontier !? (d + 1)
        goodMoves = filter
            (\p -> noneIn thisBlizzies p
                && inBounds bounds p)
            [ at
            , (x, y + 1)
            , (x, y - 1)
            , (x + 1, y)
            , (x - 1, y)
            ]

moveBlizzards :: Bounds -> Blizzards -> Blizzards
moveBlizzards bounds blizzies = Blizzards
    (S.map (wrap . second (subtract 1)) (ups blizzies))
    (S.map (wrap . first (+ 1)) (rights blizzies))
    (S.map (wrap . second (+ 1)) (downs blizzies))
    (S.map (wrap . first (subtract 1)) (lefts blizzies))
  where
    wrap = wrapBounds bounds

parseValley :: [[Char]] -> Blizzards
parseValley valley = Blizzards
    (S.fromList $ fst <$> filter ((== '^') . snd) blizzies)
    (S.fromList $ fst <$> filter ((== '>') . snd) blizzies)
    (S.fromList $ fst <$> filter ((== 'v') . snd) blizzies)
    (S.fromList $ fst <$> filter ((== '<') . snd) blizzies)
  where
    blizzies =
        [ ((x, y), at)
        | x <- [0..length (head valley) - 1]
        , y <- [0..length valley - 1]
        , let at = valley !! y !! x
        , at /= '#'
        , at /= '.'
        ]
