module Day10.Part1 (thisMain) where

import qualified Data.List  as L
import           Data.Maybe
import           System.IO

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
    let ran = runAll (lines contents) (ProgState 1 1)
    print $ sum $ findCycleSS ran <$> [20, 60, 100, 140, 180, 220]
    hClose handle

-- efficient!
findCycleSS :: [ProgState] -> Int -> Int
findCycleSS ps c = clock found * xReg found
  where
    found = fromMaybe (ProgState (-1) (-1)) (L.find ((== c) . clock) ps)

runAll :: [String] -> ProgState -> [ProgState]
runAll (si : sis) ps = runAll sis (head ran) ++ ran
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
