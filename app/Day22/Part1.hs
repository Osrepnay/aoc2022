module Day22.Part1 (thisMain) where

import           Data.Bifunctor
import           Data.Char
import qualified Data.List       as L
import           Data.List.Split
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day22" ReadMode
    contents <- hGetContents handle
    let splot = splitOn "\n\n" contents
    let gridLines = lines $ head splot
    let maxWidth = maximum $ length <$> gridLines
    let grid = rightpad maxWidth <$> gridLines
    let moves = parseMoves $ head $ lines $ last splot
    let horzRanges = gridRanges grid
    let vertRanges = gridRanges $ L.transpose grid
    let start = Position (rangeStart (head horzRanges), 0) HRight
    let finalPos = L.foldl' (doMove grid horzRanges vertRanges) start moves
    print
        $ 1000 * (snd (pos finalPos) + 1)
        + 4 * (fst (pos finalPos) + 1)
        + headingNum (heading finalPos)
    hClose handle

rightpad :: Int -> String -> String
rightpad len s = s ++ replicate (len - length s) ' '

data Range = Range
    { rangeStart :: Int
    , rangeEnd   :: Int
    } deriving Show

wrapRange :: Range -> Int -> Int
wrapRange (Range s e) n = (n - s) `mod` rangeLen + s
  where
    rangeLen = e - s + 1

data Move = Forward Int | RotateRight | RotateLeft deriving Show

data Heading = HRight | HDown | HLeft | HUp deriving Show

headingNum :: Heading -> Int
headingNum HRight = 0
headingNum HDown  = 1
headingNum HLeft  = 2
headingNum HUp    = 3

rotateRight :: Heading -> Heading
rotateRight HRight = HDown
rotateRight HDown  = HLeft
rotateRight HLeft  = HUp
rotateRight HUp    = HRight

rotateLeft :: Heading -> Heading
rotateLeft HUp    = HLeft
rotateLeft HLeft  = HDown
rotateLeft HDown  = HRight
rotateLeft HRight = HUp

data Position = Position
    { pos     :: (Int, Int)
    , heading :: Heading
    } deriving Show

doMove :: [[Char]] -> [Range] -> [Range] -> Position -> Move -> Position
doMove _ _ _ (Position p h) RotateRight = Position p (rotateRight h)
doMove _ _ _ (Position p h) RotateLeft = Position p (rotateLeft h)
doMove _ _ _ p (Forward 0) = p
doMove grid hRanges vRanges (Position (x, y) h) (Forward d)
    | grid !! snd newP !! fst newP == '#' = Position (x, y) h
    | otherwise = doMove
        grid
        hRanges
        vRanges
        (Position newP h)
        (Forward (d - 1))
  where
    thisHRange = hRanges !! y
    thisVRange = vRanges !! x
    newP = bimap
        (wrapRange thisHRange)
        (wrapRange thisVRange)
        $ case h of
            HRight -> (x + 1, y)
            HDown  -> (x, y + 1)
            HLeft  -> (x - 1, y)
            HUp    -> (x, y - 1)

gridRanges :: [[Char]] -> [Range]
gridRanges grid = headComplain . lineRanges <$> grid
  where
    headComplain [x] = x
    headComplain _   = error "more than one protrusion"

lineRanges :: String -> [Range]
lineRanges [] = []
lineRanges (' ' : l) = (\(Range s e) -> Range (s + 1) (e + 1))
    <$> lineRanges l
lineRanges l = Range 0 (length has - 1) : lineRanges empty
  where
    (has, empty) = span (/= ' ') l

parseMoves :: String -> [Move]
parseMoves [] = []
parseMoves ('R' : ms) = RotateRight : parseMoves ms
parseMoves ('L' : ms) = RotateLeft : parseMoves ms
parseMoves ms = Forward (read digs) : parseMoves rest
  where
    (digs, rest) = span isDigit ms
