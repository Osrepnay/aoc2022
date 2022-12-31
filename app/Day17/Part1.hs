module Day17.Part1 (thisMain) where

import           Data.Foldable
import           Data.Function
import           Data.Set      (Set)
import qualified Data.Set      as S
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day17" ReadMode
    contents <- hGetContents handle
    let jets = cycle $ parseJets $ head $ lines contents
    let pieces = take 2022 $ cycle [Bar, Plus, Corner, Pole, Square]
    let drops = dropPiece <$> pieces
    print
        $ (1 +)
        $ tetriiTallest
        $ snd
        $ foldl' (&) (jets, Tetrii (-1) S.empty) drops
    hClose handle

data Jet = JetLeft | JetRight deriving Show

data Tetrii = Tetrii
    { tetriiTallest  :: Int
    , tetriiOccupied :: Set (Int, Int)
    } deriving Show

data Piece = Bar | Plus | Corner | Pole | Square deriving Show

dropPiece :: Piece -> ([Jet], Tetrii) -> ([Jet], Tetrii)
dropPiece piece (jets, Tetrii tallest occupied) =
    ( newJets
    , Tetrii
        (max tallest $ snd $ maximumBy (compare `on` snd) groundedPiece)
        (S.union groundedPiece occupied)
    )
  where
    (newJets, groundedPiece) = goJet (2, tallest + 4) jets
    goDown (x, y) js
        | stop      = (js, pieceSet (x, y))
        | otherwise = goJet (x, y - 1) js
      where
        downSet = pieceSet (x, y - 1)
        stop = y <= 0 || not (S.disjoint downSet occupied)
    goJet (x, y) (j : js)
        | stop      = goDown (x, y) js
        | otherwise = goDown (jx, jy) js
      where
        (jx, jy) = case j of
            JetLeft  -> (x - 1, y)
            JetRight -> (x + 1, y)
        rjx = (jx +) $ case piece of
            Bar    -> 3
            Plus   -> 2
            Corner -> 2
            Pole   -> 0
            Square -> 1
        stop = jx < 0
            || rjx >= 7
            || not (S.disjoint (pieceSet (jx, jy)) occupied)
    goJet _ [] = error "no more jets"
    pieceSet (x, y) = case piece of
        Bar -> S.fromList [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)]
        Plus -> S.fromList
            [ (x + 1, y)
            , (x, y + 1)
            , (x + 1, y + 1)
            , (x + 2, y + 1)
            , (x + 1, y + 2)
            ]
        Corner -> S.fromList
            [ (x, y)
            , (x + 1, y)
            , (x + 2, y)
            , (x + 2, y + 1)
            , (x + 2, y + 2)
            ]
        Pole   -> S.fromList [(x, y), (x, y + 1), (x, y + 2), (x, y + 3)]
        Square -> S.fromList [(x, y), (x, y + 1), (x + 1, y), (x + 1, y + 1)]

parseJets :: String -> [Jet]
parseJets []         = []
parseJets ('<' : js) = JetLeft : parseJets js
parseJets ('>' : js) = JetRight : parseJets js
parseJets _          = error "unknown jet direction"
