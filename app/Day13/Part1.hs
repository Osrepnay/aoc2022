module Day13.Part1 (thisMain) where

import Control.Monad.Trans.State.Strict
import Data.Char
import Data.List.Split
import System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day13" ReadMode
    contents <- hGetContents handle
    let pairs = (\x -> compare (head x) (last x))
            . fmap (evalState parsePacket)
            . lines
            <$> splitOn "\n\n" contents
    print $ sum $ fmap fst $ filter ((== LT) . snd) $ zip [(1 :: Int)..] pairs
    hClose handle

data Packet = PInteger Int | PList [Packet] deriving (Eq, Show)

instance Ord Packet where
    compare (PInteger a) (PInteger b) = compare a b
    compare (PInteger a) bs           = compare (PList [PInteger a]) bs
    compare as (PInteger b)           = compare as (PList [PInteger b])
    compare (PList as) (PList bs)     = compare as bs

parsePacket :: State String Packet
parsePacket = do
    ps <- get
    let (digits, rest) = span isDigit ps
    case digits of
        [] -> do
            put (tail rest)
            PList <$> parseList
        _  -> do
            put rest
            pure $ PInteger $ read digits
  where
    parseList = do
        old <- get
        ends <- expectChar ']'
        if ends then
            pure []
        else do
            put old
            next <- parsePacket
            hasComma <- expectChar ','
            if hasComma
               then (next :) <$> parseList
               else pure [next]
    expectChar c = do
        ps <- get
        put (tail ps)
        pure ((head ps) == c)
