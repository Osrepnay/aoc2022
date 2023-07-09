module Day22.Part2 (thisMain) where

import           Control.Applicative
import           Data.Bifunctor
import           Data.Char
import           Data.List           ((\\))
import qualified Data.List           as L
import           Data.List.Split
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Tuple
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day22" ReadMode
    contents <- hGetContents handle
    let splot = splitOn "\n\n" contents
    let gridLines = lines $ head splot
    let maxWidth = maximum $ length <$> gridLines
    let grid = rightpad maxWidth <$> gridLines
    let faceSize = minimum $ length . filter (/= ' ') <$> grid
    let gridFaces = L.transpose . fmap (chunksOf faceSize) <$> chunksOf faceSize grid
    let gridFacesFlat = filter ((/= ' ') . head . head . snd)
            $ concat
            $ zipWith (\r xs -> (\(c, x) -> ((r, c), x)) <$> xs)
                [0..]
                (zip [0..] <$> gridFaces)
    print (gridFacesFlat !! 5)
    let context = fillContext (trivialContext (tagFaces gridFaces))
    mapM_ print $ M.toList context
    let moves = parseMoves $ head $ lines $ last splot
    let (face, (row, col), side) = followMoves (snd <$> gridFacesFlat) context moves
    let (faceRow, faceCol) = fst (gridFacesFlat !! face)
    let finalRow = faceRow * faceSize + row + 1
    let finalCol = faceCol * faceSize + col + 1
    print $ 1000 * finalRow + 4 * finalCol + case side of
        SideTop    -> 3
        SideLeft   -> 2
        SideBottom -> 1
        SideRight  -> 0
    hClose handle

rightpad :: Int -> String -> String
rightpad len s = s ++ replicate (len - length s) ' '

data Side = SideTop | SideLeft | SideBottom | SideRight deriving (Eq, Show)
type FaceId = Int
type Context = Map FaceId [(Side, (Side, FaceId))]
data Move = Forward Int | RotateRight | RotateLeft deriving Show

followMoves :: [[String]] -> Context -> [Move] -> (Int, (Int, Int), Side)
followMoves faces context moves = go moves 0 (0, 0) SideRight
  where
    faceSize = length (head faces)
    sideDelta SideTop    = (-1,  0)
    sideDelta SideLeft   = ( 0, -1)
    sideDelta SideBottom = ( 1,  0)
    sideDelta SideRight  = ( 0,  1)
    flipSides a b
        | a == SideLeft || a == SideBottom = not (b == SideRight || b == SideTop)
        | b == SideLeft || b == SideBottom = not (a == SideRight || a == SideTop)
        | otherwise = True
    wrapSides sf st r c
        | r < 0 || r >= faceSize = wrapSides sf st overflowed (maybeFlip c)
        | c < 0 || c >= faceSize = wrapSides sf st (maybeFlip r) overflowed
        | otherwise = maybeSwap (r, c)
      where
        maybeFlip = if flipSides sf st then (\x -> faceSize - x - 1) else id
        maybeSwap = if isOrthogonal sf st then swap else id
        overflowed = if st == SideTop || st == SideLeft then 0 else faceSize - 1
    go :: [Move] -> Int -> (Int, Int) -> Side -> (Int, (Int, Int), Side)
    go [] faceId pos heading = (faceId, pos, heading)
    go (m : ms) faceId (row, col) heading = case m of
        Forward 0 -> go ms faceId (row, col) heading
        Forward n ->
            let (dr, dc) = sideDelta heading
                (fid, (ns, nr, nc)) = wrap faceId (row + dr) (col + dc)
            in if faces !! fid !! nr !! nc == '#'
            then go ms faceId (row, col) heading
            else go (Forward (n - 1) : ms) fid (nr, nc) ns
        RotateRight -> go ms faceId (row, col) (clockwiseSide heading)
        RotateLeft  -> go ms faceId (row, col) (counterSide heading)
      where
        wrap fid r c
            | r < 0 || c < 0 || r >= faceSize || c >= faceSize =
                let (s, nfid) = fromJust (lookup heading (context M.! fid))
                    (nr, nc) = wrapSides heading s r c
                in (nfid, (oppositeSide s, nr, nc))
            | otherwise = (fid, (heading, r, c))

clockwiseSide :: Side -> Side
clockwiseSide SideTop    = SideRight
clockwiseSide SideLeft   = SideTop
clockwiseSide SideBottom = SideLeft
clockwiseSide SideRight  = SideBottom

counterSide :: Side -> Side
counterSide = clockwiseSide . clockwiseSide . clockwiseSide

oppositeSide :: Side -> Side
oppositeSide = clockwiseSide . clockwiseSide

isOrthogonal :: Side -> Side -> Bool
isOrthogonal a b = a == clockwiseSide b || clockwiseSide a == b

isOpposing :: Side -> Side -> Bool
isOpposing a b = a /= b && not (isOrthogonal a b)

tagFaces :: [[[String]]] -> [[(FaceId, [String])]]
tagFaces faces = fst $ L.foldl'
    (\(bs, c0) -> first ((bs ++) . L.singleton) . L.foldl'
        (\(b, c1) a ->
            if head (head a) == ' '
            then (b ++ [(-1, a)], c1)
            else (b ++ [(c1, a)], c1 + 1))
        ([], c0))
    ([], 0)
    faces

allSides :: [(Side, (Int, Int))]
allSides = [(SideTop, (-1, 0)), (SideLeft, (0, -1)), (SideBottom, (1, 0)), (SideRight, (0, 1))]

trivialContext :: [[(FaceId, [String])]] -> Context
trivialContext faces = M.fromList $ catMaybes
    [ case fst face of
        -1     -> Nothing
        faceId -> Just
            ( faceId
            , second (second fst)
            <$> filter
                ((/= -1) . fst . snd . snd)
                (second (second (\(nr, nc) -> faces !! nr !! nc)) <$> neighbors r c)
            )
    | r <- [0..length faces - 1]
    , c <- [0..length (head faces) - 1]
    , let face = faces !! r !! c
    ]
  where
    neighbors r c = filter
        (\(_, (_, (nr, nc))) -> nr >= 0 && nc >= 0 && nr < length faces && nc < length (head faces))
        ((\(s, x) -> (s, (clockwiseSide (clockwiseSide s), x))) . second (bimap (+ r) (+ c)) <$> allSides)

fillContext :: Context -> Context
fillContext context = M.unionWith (++) context
    $ M.mapWithKey
        (\f cs -> (\s -> (s, fromJust (fillSide f s)))
            <$> [SideTop, SideLeft, SideBottom, SideRight] \\ (fst <$> cs))
        context
  where
    fillSide faceId originSide = go faceId S.empty [originSide]
      where
        faceDiffClockwise SideLeft SideTop   = True
        faceDiffClockwise SideLeft SideBottom = False
        faceDiffClockwise SideRight dir = not (faceDiffClockwise SideLeft dir)
        faceDiffClockwise dir0 dir1 = not (faceDiffClockwise dir1 dir0)
        faceDiffRotate side0 side1 = if faceDiffClockwise side0 side1 then clockwiseSide else counterSide
        go :: Int -> Set Int -> [Side] -> Maybe (Side, FaceId)
        go currFace visited sidesWant = case singleWant of
            Just x  -> Just x
            Nothing -> case M.lookup currFace context of
                Nothing -> Nothing
                Just fs -> asum
                    $ (\(fromSide, (_, toFace)) ->
                        if isOpposing (head sidesWant) fromSide
                        then go toFace nvisit (head sidesWant : sidesWant)
                        else first (faceDiffRotate (head sidesWant) fromSide)
                            <$> go toFace nvisit sidesWant)
                    <$> filter (not . (`S.member` visited) . snd . snd) fs
          where
            nvisit = S.insert currFace visited
            singleWant = case sidesWant of
                [s] -> M.lookup currFace context >>= lookup s
                [a, b] -> if a == b
                    then M.lookup currFace context
                        >>= asum
                        . fmap (\(fromSide, (_, toFace)) -> first (faceDiffRotate a fromSide) <$> go toFace nvisit [fromSide])
                        . filter (\(s, (_, f)) -> (s == clockwiseSide a || s == counterSide a) && not (f `S.member` visited))
                    else Nothing
                [a, b, c] -> if a == b && b == c
                    then M.lookup currFace context >>= lookup (oppositeSide a)
                    else Nothing
                _ -> Nothing

parseMoves :: String -> [Move]
parseMoves [] = []
parseMoves ('R' : ms) = RotateRight : parseMoves ms
parseMoves ('L' : ms) = RotateLeft : parseMoves ms
parseMoves ms = Forward (read digs) : parseMoves rest
  where
    (digs, rest) = span isDigit ms
