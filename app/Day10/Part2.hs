module Day10.Part2 (thisMain) where

import Data.List.Split
import System.IO

data ProgState = ProgState
    { xReg  :: Int
    , clock :: Int
    } deriving (Show)

addToX :: Int -> ProgState -> ProgState
addToX dx (ProgState x c) = ProgState (x + dx) c

incClock :: ProgState -> ProgState
incClock (ProgState x c) = ProgState x (c + 1)

data Instr = Addx Int | Noop

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day10" ReadMode
    contents <- hGetContents handle
    let ran = (ProgState 1 1) : runAll (lines contents) (ProgState 1 1)
    let chunked = chunksOf 40 ran
    let view =
            (\line ->
                (\(x, ps) ->
                    if abs (x - (xReg ps)) < 2
                       then '#'
                       else '.')
                    <$> zip [0..39] line) <$> chunked
    putStrLn $ unlines view
    hClose handle

runAll :: [String] -> ProgState -> [ProgState]
runAll (si : sis) ps = reverse ran ++ runAll sis (head ran)
  where
    ran = runInstr instruction ps
    instruction
        | head worded == "addx" = Addx $ read $ last $ worded
        | head worded == "noop" = Noop
        | otherwise             = error "unknown instruction"
    worded = words si
runAll [] _ = []

runInstr :: Instr -> ProgState -> [ProgState]
runInstr i state = incClock <$>
    case i of
        Addx dx -> [incClock (addToX dx state), state]
        Noop    -> [state]
