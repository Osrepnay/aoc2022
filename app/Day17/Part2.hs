module Day17.Part2 (thisMain) where

import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Map       (Map, (!))
import qualified Data.Map       as M
import           Data.Set       (Set)
import qualified Data.Set       as S
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day17" ReadMode
    contents <- hGetContents handle
    let jetsNoCycle = parseJets $ head $ lines contents
    let jetsNoCycleZ = zip [0..length jetsNoCycle - 1] jetsNoCycle
    let jets = cycle jetsNoCycleZ
    let (offset, cycleLen) = dropUntilCycle
            0
            Bar
            M.empty
            (jets, Tetrii (-1) S.empty)
    let totalDrop = 1000000000000
    let (starting, sp) = dropPieces offset Bar (jets, Tetrii (-1) S.empty)
    let rsh = tetriiTallest (snd starting)
    let sh = rsh
    let (cycled, cp) = dropPieces cycleLen sp starting
    let rch = tetriiTallest (snd cycled)
    let ch = rch - sh
    let toCycle = (totalDrop - offset) `div` cycleLen
    let left = (totalDrop - offset) `mod` cycleLen
    let (done, _) = dropPieces left cp cycled
    let rdh = tetriiTallest (snd done)
    let dh = rdh - ch - sh
    print $ 1 + sh + toCycle * ch + dh
    hClose handle

data Jet = JetLeft | JetRight deriving Show

data Tetrii = Tetrii
    { tetriiTallest  :: Int
    , tetriiOccupied :: Set (Int, Int)
    } deriving Show

data Piece = Bar | Plus | Corner | Pole | Square deriving (Eq, Ord, Show)

normalize :: Set (Int, Int) -> Set (Int, Int)
normalize s = S.map (second (subtract lowest)) s
  where
    lowest = snd (minimumBy (compare `on` snd) s)

edges :: Tetrii -> Set (Int, Int)
edges (Tetrii tallest occupied) = S.intersection occupied (fill (tallest + 1) occupied)

fill :: Int -> Set (Int, Int) -> Set (Int, Int)
fill t occ = go (0, t) S.empty
  where
    go p@(x, y) vis
        | oob            = vInserted
        | S.member p occ = vInserted
        | S.member p vis = vInserted
        | otherwise      = foldl'
            (flip go)
            vInserted
            [ (x - 1, y)
            , (x + 1, y)
            , (x, y - 1)
            , (x, y + 1)
            ]
      where
        vInserted = S.insert p vis
        oob = x < 0 || x >= 7 || y < 0 || y > t

dropPieces
    :: Int
    -> Piece
    -> ([(Int, Jet)], Tetrii)
    -> (([(Int, Jet)], Tetrii), Piece)
dropPieces 0 p s = (s, p)
dropPieces n p s = dropPieces (n - 1) next (dropPiece p s)
  where
    next = case p of
        Bar    -> Plus
        Plus   -> Corner
        Corner -> Pole
        Pole   -> Square
        Square -> Bar

dropUntilCycle
    :: Int
    -> Piece
    -> Map (Piece, Int, Set (Int, Int)) Int
    -> ([(Int, Jet)], Tetrii)
    -> (Int, Int)
dropUntilCycle n p dm (js, t)
    | M.member key dm = (dm ! key, n - dm ! key)
    | n == 1422 = dropUntilCycle
        (n + 1)
        next
        (M.insert key n dm)
        (dropPiece p (js, t))

    | otherwise       = dropUntilCycle
        (n + 1)
        next
        (M.insert key n dm)
        (dropPiece p (js, t))
  where
    key = (p, fst (head js), normalize (edges t))
    next = case p of
        Bar    -> Plus
        Plus   -> Corner
        Corner -> Pole
        Pole   -> Square
        Square -> Bar

dropPiece :: Piece -> ([(Int, Jet)], Tetrii) -> ([(Int, Jet)], Tetrii)
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
        (jx, jy) = case snd j of
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
